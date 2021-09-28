------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2021, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- This package handles the transition from Compilation to the final result,
-- and is compiler-specific. This particular package is GNAT-specific

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

package Build.Linking is
   
   package UBSQ is new Ada.Containers.Synchronized_Queue_Interfaces
     (UBS.Unbounded_String);
   
   package Unbounded_String_Queues is 
     new Ada.Containers.Unbounded_Synchronized_Queues (UBSQ);
   
   ------------------
   -- All_Compiled --
   ------------------
   
   -- Common precondition
   
   use type Registrar.Library_Units.Library_Unit_State;
   
   function All_Compiled 
     (Unit_Set: Registrar.Library_Units.Library_Unit_Sets.Set)
     return Boolean 
   is (for all Unit of Unit_Set => 
         Unit.State = Registrar.Library_Units.Compiled);
   
   -------------------------
   -- Scan_Linker_Options --
   -------------------------
   
   
   procedure Scan_Linker_Options 
     (Unit_Set: in Registrar.Library_Units.Library_Unit_Sets.Set)
   with Pre => All_Compiled (Unit_Set);
   
   Scan_Progress : aliased Progress.Progress_Tracker;
   Linker_Options: Unbounded_String_Queues.Queue;
   
   -- Scans all Units in Unit_Set, and adds any linker options retrieved to
   -- the Linker_Options queue.
   
   ----------
   -- Bind --
   ----------
   
   procedure Bind
     (Unit_Set     : in     Registrar.Library_Units.Library_Unit_Sets.Set;
      Configuration: in     Build_Configuration;
      Errors       :    out UBS.Unbounded_String)
   with Pre => All_Compiled (Unit_Set);
   -- Generates and enters the binder source (unit name is "ada_main" for GNAT)
   --
   -- After calling Bind, and waiting for unit entry to complete, another
   -- compilation run is needed to compile the binder source.
   --
   -- If the process is successful, Errors will have a zero length
   
   ----------
   -- Link --
   ----------
   
   procedure Link_Image 
     (Image_Path   : in     String;
      Unit_Set     : in     Registrar.Library_Units.Library_Unit_Sets.Set;
      Configuration: in     Build_Configuration;
      Errors       :    out UBS.Unbounded_String)
   with Pre => Configuration.Mode in Image | Library
               and All_Compiled (Unit_Set);
   
   -- Shall be called after Bind.
   -- Shall be called after Scan_Linker_Options has completed on Unit_Set.
   --
   -- This is the last phase of a build process.
   --
   -- Link_Image creates an executable image that is put in the project root
   -- directory, and has the file name of Image_Name. It incorproates only
   -- the object files for the units of Unit_Set.
   
   procedure Link_Library 
     (Library_Path : in     String;
      Unit_Set     : in     Registrar.Library_Units.Library_Unit_Sets.Set;
      Configuration: in     Build_Configuration;
      Errors       :    out UBS.Unbounded_String)
     renames Link_Image;
   
   -- Shall be called after Bind.
   -- Shall be called after Scan_Linker_Options has completed on Unit_Set.
   --
   -- This is the last phase of a build process.
   --
   -- Links all compilation objects of Unit_Set into a single dynamic library,
   -- of the file specified by Library_Path.
   --
   -- In the case of GNAT/gcc, linking a shared library is exactly the same
   -- as linking an image, except for the extension of the output file
   
   procedure Archive 
     (Archive_Path : in     String;
      Unit_Set     : in     Registrar.Library_Units.Library_Unit_Sets.Set;
      Configuration: in     Build_Configuration;
      Errors       :    out UBS.Unbounded_String)
   with Pre => Configuration.Mode = Library
               and All_Compiled (Unit_Set);
   
   -- Shall be called after Bind.
   -- Shall be called after Scan_Linker_Options has completed on Unit_Set.
   --
   -- Creates an archive file in the project root directory named Base_Name.a
   -- that contains all compilation objects of Unit_Set. Also generates
   -- a Base_Name.linkopt that contains a string indicating all required
   -- libraries and linker options loaded from Scan_Linker_Options.
   
   ---------------------
   -- Link_Subsystems --
   ---------------------
   
   procedure Link_Subsystems;
   Link_Subsystems_Progress: aliased Progress.Progress_Tracker;
   Link_Subsystems_Errors  : Unbounded_String_Queues.Queue;
   
   -- Link_Subsystems executes its own series of Linker_Option scans that are
   -- specific to each registered Subsystem, and so Scan_Linker_Options
   -- does NOT need to be run.
   --
   -- Link_Subsystems Creates a Ada-specific shared library for each registered
   -- Subsystem, and places a shared library with the name of the subsystem
   -- in the build root.
   --
   -- Linker command output is per subsystem
   
end Build.Linking;
