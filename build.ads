------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2023, ANNEXI-STRAYLINE Inc.                          --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--                                                                          --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT      --
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        --
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   --
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   --
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     --
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   --
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    --
--                                                                          --
------------------------------------------------------------------------------

-- This subsystem handles all operations associated with compilation and
-- linking

with Ada.Directories;
with Ada.Strings.Unbounded;

with Progress;
with Registrar.Library_Units;
with Child_Processes.Wait_And_Buffer;

package Build is
   
   package UBS renames Ada.Strings.Unbounded;
   
   -------------------------
   -- Build_Configuration --
   -------------------------
   
   -- Various global configuration data for the build process, set from the
   -- command line. This configuration affects the compilation artifacts,
   -- and so is stored between successive runs. If any of the build
   -- configuration changes, 
   
   type Build_Mode is 
     (Systemize,
      -- The build process will be creating a System repository out of the
      -- project
      
      Library,
      -- The build process will be creating a stand-alone shared library, or
      -- an archive of all compilation objects
      
      Image);
      -- The build process will be creating a standard executable image
   
   type Link_Mode is 
     (Shared,
      -- Link to as many shared libraries as possible. This requires "pic"
      -- during compilation.
      
      Static_RT, 
      -- Ada runtime is linked statically, as well as libgcc in the case of
      -- GNAT
      
      Static);
      -- Build a fully statically-linked image. Applicable only to Image mode
      -- builds.
   
   type Compile_Optimization_Mode is
     (None,
     -- No optimization (typically means -O0, unless set by the compiler
     -- options of any configuration units
     
      Level_1,
      Level_2,
      Level_3,
      -- The classic -O1 -O2 -O3 optimization levels
      
      Size,
      -- Optimize for size (-Os)
      
      Debug);
       -- Optimize for debugging (-O0)
   
   
   type Build_Configuration is
      record
         Mode: Build_Mode := Image;
         
         Position_Independent: Boolean := True;
         -- Use -fPIC for compilation and -fPIE for linking
         -- if False, use -fno-PIC and -fno-PIE
         --
         -- This value must be true for Shared and Static_RT Link_Modes,
         -- or if Mode is Systemize
         
         Debug_Enabled: Boolean := False;
         -- If True, '-g' is passed to the compiler and linker.
         
         All_Assertions: Boolean := False;
         -- If True, forces all assertions on for all Ada units
         
         Optimization: Compile_Optimization_Mode := None;
         
         Linking: Link_Mode := Shared;
      end record;
   
   procedure Load_Last_Build_Config (Configuration: out Build_Configuration);
   -- Loads the last run's build configuration, if available. If not available,
   -- Configuration is not modified
   
   procedure Store_Build_Config (Current_Config: in Build_Configuration);
   -- Stores Current_Config such that Load_Last_Build_Config will retrive the
   -- stored data
   
   -----------------
   -- Preparation --
   -----------------
   
   procedure Init_Paths;
   -- Shall be called before executing any of the build steps
   -- (Compile, Bind, Link).
   --
   -- Ensures that the Build_Root and Build_Output_Root paths exist.
   --
   -- If Last_Run is empty, the Build_Root is first destroyed.
   
   procedure Hash_Compilation_Products;
   Compilation_Hash_Progress: aliased Progress.Progress_Tracker;
   
   -- Searches for and hashes any existing compilation products for units with
   -- a state of "Available. All such units that have compilation units will be
   -- set to a state of "Compiled", indicating the Compilation_Hash value is
   -- valid. 
   
   procedure Compute_Recompilations (Configuration: Build_Configuration);
   Compute_Recompilations_Progress  : aliased Progress.Progress_Tracker;
   Compute_Recompilations_Completion:         Progress.Sequence_Beacon;
   
   -- Compare existing Source and Compilation Hashes (if State = Compiled) with
   -- Last_Run to determine which units need to be recompiled. After
   -- Compute_Recompilations completes, any units that need recompilation will
   -- be changed to a state of "Available".
   --
   -- If any specification is modified, or if an object file is invalidated,
   -- a "pessemistic" recompilation is computed - this triggers recursive
   -- recompilation of all units depending on the unit to be pressemistically
   -- recompiled (including dependents of the dependents).
   --
   -- If only an implementation of a target unit has changed between
   -- compilations, an attempt is made to approve an "isolated" recompilation.
   -- In the case of GNAT, this means scanning the specification of the target
   -- unit for any generic declarations or inlined subprograms. If any generic
   -- declarations or inlined subprograms are found, a "pessemistic"
   -- recompilation is computed, otherwise the "isolated" recompilation will
   -- select only the target unit for recompilation.
   --
   -- If 
   --    * Last_Run.All_Library_Units is an empty set; or,
   --    * Configuration is different from Load_Last_Build_Config
   -- all units with a state of "Compiled" will be set to Available (except for
   -- units that are members of subsystems that are checked out from "system"
   -- repositories) - causing them to be recompiled.
   --
   -- The Compute_Recompilation process has two phases. Phase one is tracked by
   -- the Compute_Recompilations_Progress tracker. Phase two is executed
   -- by a Phase Trigger. On call to Compute_Recompilations, the
   -- Compute_Recompilations_Completion beacon is entered, and is the left
   -- and the completion of the phase trigger.
   --
   -- The user should for the beacon to be left before continuing
   --
   -- If approach of the beacon fails, Compute_Recompilation returns
   -- immediately.
   

   -- Build_Root --
   ----------------
   -- The root subdirectory under which all compilation products and information
   -- is stored
   
   Build_Root: constant String 
     := Ada.Directories.Current_Directory & "/aura-build";
   -- This directory is always destroyed in the Compile phase if Last_Run
   -- is empty
   
private
   
   Build_Output_Root: constant String := Build_Root & "/build-output";
   -- Output from all processes involved in the build process.
   
   procedure Wait_And_Buffer is new Child_Processes.Wait_And_Buffer
     (Buffer_Type  => UBS.Unbounded_String,
      Append       => UBS.Append,
      Empty_Buffer => UBS.Null_Unbounded_String);
   
   procedure Direct_Hash_Compilation_Products
     (Unit: in Registrar.Library_Units.Library_Unit);
   -- Directly hashes
   
   use type Registrar.Library_Units.Library_Unit_Kind;
   
   function Object_File_Name (Unit: Registrar.Library_Units.Library_Unit)
                             return String with 
     Pre => Unit.Kind /= Registrar.Library_Units.Subunit;
   -- Computes the Full Name of the object file that is expected to be inside
   -- of Build_Root, for the designated unit.
   --
   -- If the designated unit is a member of an AURA Subsystem that belongs
   -- to a "System" Repository, then the expected name of the shared library
   -- object is given
   
   function ALI_File_Name (Unit: Registrar.Library_Units.Library_Unit)
                          return String with 
     Pre => Unit.Kind not in Registrar.Library_Units.Subunit;
   -- Computes the Full Name of an ALI file (GNAT-specific) that is expected
   -- to be inside of Build_Root, for the designated unit
   
end Build;
