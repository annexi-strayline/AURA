------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

with Ada.Text_IO;
with Ada.Directories;
with Ada.Assertions;
with Ada.Strings.Wide_Wide_Fixed;

with Unit_Names;
with Platform_Info;

with Repositories;
with Registrar.Queries;
with Registrar.Last_Run;
with Registrar.Subsystems;
with Registrar.Registration;
with Registrar.Library_Units;
with Registrar.Source_Files;
with Workers, Workers.Reporting;
with Child_Processes.Path_Searching;

package body Build.Compilation is
   
   procedure Assert (Check: in Boolean; Message: in String)
     renames Ada.Assertions.Assert;
   
   New_Line: Character renames Workers.Reporting.New_Line;
   
   package Program_Paths renames Child_Processes.Path_Searching;
   
   Compiler_Program: aliased constant String
     := Platform_Info.Toolchain_Prefix & "gcc";
   Compiler: constant Program_Paths.Elaboration_Path_Search
     := Program_Paths.Initialize (Compiler_Program);
   
   --
   -- Compilation Order Support Utilities
   -- 
   
   -- Select_Configuration_Subsystem --
   ------------------------------------
   
   -- Basically, for non-AURA subsystem units, we need to select the AURA
   -- subsystem
   
   function Select_Configuration_Subsystem 
     (Unit: Registrar.Library_Units.Library_Unit)
     return Registrar.Subsystems.Subsystem
   is
      use Registrar.Subsystems;
      
   begin
      return Unit_Subsys: Subsystem 
          := Registrar.Queries.Lookup_Subsystem (Unit.Name.Subsystem_Name)
      do
         if not Unit_Subsys.AURA then
            Unit_Subsys := Registrar.Queries.Lookup_Subsystem 
              (Unit_Names.Set_Name ("aura"));
         end if;
         
      end return;
   end Select_Configuration_Subsystem;
   
   
   -- Compute_Arguments --
   -----------------------
   
   -- Compute all of the arguments needed to pass to the compiler to compile
   -- Unit.
   --
   -- Opportunistically determine if the Unit is a member of a "System" format
   -- Repository

   function Compute_Arguments
     (Unit            : in     Registrar.Library_Units.Library_Unit;
      Config          : in     Build_Configuration;
      From_System_Repo:    out Boolean)
     return String
   is
      package WWF renames Ada.Strings.Wide_Wide_Fixed;
      use type Registrar.Source_Files.Source_File_Access;
      
      use UBS;
      use Repositories;
      use Registrar.Subsystems;
      use Registrar.Library_Units;
      
      Unit_Subsys: constant Subsystem := Select_Configuration_Subsystem (Unit);
      Available_Subsystems: constant Subsystem_Sets.Set 
        := Registrar.Queries.Available_Subsystems;
      
      Buffer: Unbounded_String := To_Unbounded_String ("-c");
      
      procedure Add_Codepaths (Subsys: in Subsystem) is
         -- Append includes for the subsystem and all configured codepaths to
         -- the buffer
         
         Subsys_Path: constant String :=  "../" & Subsys.Name.To_UTF8_String;
      begin
         Append (Buffer, " -I" & Subsys_Path);
         
         for Codepath_Pair of Subsys.Configuration.Codepaths loop
            Append (Buffer, " -I" & Subsys_Path & '/' 
                      & UBS.To_String (Codepath_Pair.Value));
         end loop;
      end Add_Codepaths;
      
   begin
      pragma Assert (Unit.State /= Requested 
                       and then Unit.Kind not in Unknown | Subunit);
      
      
      -- We take advantage of pulling the unit's subsystem to look for
      -- units from "System" repositories. Such units are not to be compiled
      
      if Unit_Subsys.AURA and then
        Extract_Repository(Unit_Subsys.Source_Repository).Format = System
      then
         From_System_Repo := True;
         return "";
      else
         From_System_Repo := False;
      end if;
      
      -- Includes. Assume the "current directory" of the compiler command
      -- is Build_Root (and therefore the root directory is at ../)
      
      -- First include our own subsystem, and codepaths for AURA, or
      -- the project root for non-aura
      
      -- (See comment below as to why the next block is commented-out)
      
--      if Unit_Subsys.AURA then
--         Add_Codepaths (Unit_Subsys);
--      else
--         Append (Buffer, " -I../");
--      end if;
      
      -- Always include the project root, but also the aura subdirectory
      -- since that contains the configuration units
      Append (Buffer, " -I../");
      Append (Buffer, " -I../aura/");
      
      -- Now the subsystems of all dependencies of this unit, and their
      -- codepaths. Obviously we don't want to needlessly add the same
      -- subsystems over and over, so we'll create a set a subset
      
      
      -- Note that the below is the "original" code. However, this approach,
      -- though more efficient, is not always compatbile with GNAT. If other
      -- toolchains can handle this approach, then this code can be selectively
      -- reactivated at a later time.
      
            
--      declare
--         Dependent_Subsys: Subsystem_Sets.Set;
--      begin
--      
--         for Dep of Registrar.Queries.Unit_Dependencies (Unit.Name) loop
--            declare
--               Dep_Subsys: constant Subsystem_Sets.Cursor
--                 := Available_Subsystems.Find 
--                   (Subsystem'(Name => Dep.Subsystem_Name, others => <>));
--            begin
--               Dependent_Subsys.Include (Subsystem_Sets.Element (Dep_Subsys));
--            end;
--         end loop;
--         
--         for Subsys of Dependent_Subsys loop
--            Add_Codepaths (Subsys);
--         end loop;
--      end;
      

      -- The issue is that GNAT uses a "macro expansion" approach to generics.
      -- This means that generic library units cannot truly be "separately
      -- compiled", and where these units (or other public generics) are used,
      -- the dependencies of those generics because virtual dependencies of the
      -- unit which instantiates the generic.
      --
      -- After some deep consideration, it was decided that the trade-off of
      -- simply including the codepaths for all needed Subsystems for the
      -- compilation of every Ada unit was not signficant enough to justify the
      -- much more intense approach to do dependency hoisting for generic
      -- instantiations, which would be a very singificant effort.

      
      for Subsys of Registrar.Queries.Available_Subsystems loop
         -- At the compilation stage, all required subsystems should be
         -- available.
         
         -- Don't include non-aura subsystems, since they are all in the
         -- root directory
         if Subsys.AURA then
            Add_Codepaths (Subsys);
         end if;
      end loop;
      
      -- For external units, we (for now) we support only c files
      if Unit.Kind = External_Unit then
         Assert (Check   => WWF.Tail (Source => Unit.Name.To_String,
                                      Count  => 2) = ".c",
                 Message => "Only ""C"" External Units can be compiled");
         
         -- Definitions
         for Def_Pair of Unit_Subsys.Configuration.C_Definitions loop
            Append (Buffer, " -D");
            Append (Buffer, Def_Pair.Value);
         end loop;
         
         -- Compiler options
         for Opt_Pair of Unit_Subsys.Configuration.C_Compiler_Opts loop
            Append (Buffer, ' ' & UBS.To_String (Opt_Pair.Value));
         end loop;
         
      else
         -- Must be Ada
         
         -- Compiler options
         for Opt_Pair of Unit_Subsys.Configuration.Ada_Compiler_Opts loop
            Append (Buffer, ' ' & UBS.To_String (Opt_Pair.Value));
         end loop;

      end if;
      
      -- Now add the global config options (currently GNAT-specific) - this
      -- makes sure that they override any others
      
      if Config.Position_Independent then
         Append (Buffer, " -fPIC");
      else
         Append (Buffer, " -fno-PIC");
      end if;
      
      if Config.Debug_Enabled then
         Append (Buffer, " -g");
      end if;
      
      if Config.All_Assertions and then Unit.Kind /= External_Unit then
         Append (Buffer, " -gnata");
      end if;
      
      case Config.Optimization is
         when None    => null;
            
         when Level_1 => Append (Buffer, " -O1");
         when Level_2 => Append (Buffer, " -O2");
         when Level_3 => Append (Buffer, " -O3");
            
         when Size    => Append (Buffer, " -Os");
         when Debug   => Append (Buffer, " -Og");
      end case;
      
      
      -- Finally append the file name
      if Unit.Body_File /= null then
         -- This applies to external units and ada units
         Append (Buffer, ' ' & Unit.Body_File.Full_Name);
      else
         pragma Assert (Unit.Kind in Package_Unit | Subprogram_Unit);
         Append (Buffer, ' ' & Unit.Spec_File.Full_Name);
      end if;
         
      return To_String (Buffer);
      
   end Compute_Arguments;
   
   
   ------------------------
   -- Compilation_Orders --
   ------------------------
   
   type Compilation_Order is new Workers.Work_Order with
      record
         Unit  : Registrar.Library_Units.Library_Unit;
         Config: Build_Configuration;
      end record;
   
   overriding function  Image   (Order: Compilation_Order) return String;
   overriding procedure Execute (Order: in out Compilation_Order);
   
   function  Image (Order: Compilation_Order) return String is
     (    "[Compilation_Order] (Build.Compilation)" & New_Line
        & " Unit: " & Order.Unit.Name.To_UTF8_String);
   
   procedure Execute (Order: in out Compilation_Order) is
      package TIO renames Ada.Text_IO;
      
      Unit: Registrar.Library_Units.Library_Unit
        renames Order.Unit;
      
      Unit_Name: constant String := Unit.Name.To_UTF8_String;
      
      From_System_Repo: Boolean;
      Args: constant String
        := Compute_Arguments (Unit, Order.Config, From_System_Repo);

   begin
      -- If the unit is a member of a subsystem that is checked out from a
      -- "System" format Repository, we never actually compile it - rather
      -- the reverse dependencies are triggered. In order to keep that 
      -- considerably more complicated process (Compute_Recompilations),
      -- it treats such units the same as all others. 
      --
      -- Therefore if we find that we have such a unit, we just rehash the
      -- compilation products (thus promoting the unit to State = Compiled),
      -- and complete the order.
      --
      -- It just so happens that Compute_Arguments needs to look at the
      -- owning subsystem anyways. Therefore the actual logic to check this
      -- is in that function, to improve efficiency
      
      if From_System_Repo then
         Direct_Hash_Compilation_Products (Unit);
         return;
      end if;
      
      
      -- Start by writing out the command line
      
      declare
         use TIO;
         CMD_OUT: File_Type;
      begin
         Create (File => CMD_OUT,
                 Name => Build_Output_Root & '/' 
                   & Unit_Name & ".cmd");
         Put_Line (CMD_OUT, "Compiler used:");
         Put_Line (CMD_OUT, Program_Paths.Image_Path (Compiler));
         Put_Line (CMD_OUT, "Arguments used:");
         Put_Line (CMD_OUT, Args);
         Close (CMD_OUT);
      end;
      
      -- Now do the compilation!
      declare
         use Child_Processes;
         
         Compile_Process: Child_Process'Class := Spawn_Process
           (Image_Path        => Program_Paths.Image_Path (Compiler),
            Arguments         => Args,
            Working_Directory => Build_Root);
         
         Timed_Out: Boolean;
         Status   : Exit_Status;
         STDOUT_Buffer, STDERR_Buffer: UBS.Unbounded_String;
      begin
         Wait_And_Buffer (Process   => Compile_Process,
                          Poll_Rate => 0.2,
                          Timeout   => 600.0,   -- Aggressive..
                          Output    => STDOUT_Buffer,
                          Error     => STDERR_Buffer,
                          Timed_Out => Timed_Out,
                          Status    => Status);
         
         if not Compile_Process.Terminated then
            Compile_Process.Kill;
            delay 0.5; -- Should be more than long enough!
            if not Compile_Process.Terminated then
               Compile_Process.Nuke;
            end if;
         end if;
         
         -- Dump stdout and stderr (if non-empty) to their files
         declare
            use TIO;
            STDOUT_File, STDERR_File: File_Type;
         begin
            
            if UBS.Length (STDOUT_Buffer) > 0 then
               Create (File => STDOUT_File,
                       Name => Build_Output_Root & '/'
                         & Unit_Name & ".out");
               
               Put (STDOUT_File, UBS.To_String (STDOUT_Buffer));               
               Close (STDOUT_File);
            end if;
            
            
            if UBS.Length (STDERR_Buffer) > 0 then
               Create (File => STDERR_File,
                       Name => Build_Output_Root & '/'
                         & Unit_Name & ".err");
               Put (STDERR_File, UBS.To_String (STDERR_Buffer));
               Close (STDERR_File);
            end if;
         end;
         
         -- Timeout is a fatal error - not your "typical" compilation
         -- failure
         Assert (Check   => not Timed_Out,
                 Message => "Compilation timed-out. "
                   &        "Check compiler output files.");
         
         -- Unsuccessful exit status generally means the compilation failed,
         -- A non-empty STDERR buffer means warnings. Both mean a "common"
         -- failure that needs to be intercepted (avoiding a work report),
         -- and a submission of the error output to the Compilation_Messages
         -- queue
         
         if UBS.Length (STDERR_Buffer) > 0 then
            Compiler_Messages.Enqueue
              (Compiler_Message'(Unit    => Unit,
                                 Context => (if Status = Success then
                                                Warning
                                             else
                                                Error),
                                 STDERR  => STDERR_Buffer));
         end if;
         
         
         if Status /= Success then
            -- Only actual non-successful exit statuses warrent a "failure"
            
            -- Deatch the tracker so that the worker won't touch it, and then
            -- increment failure. There is no phase trigger for compilation
            -- orders
            
            Order.Tracker.Increment_Failed_Items;
            Order.Tracker := null;
         end if;
         
         if Status = Success then
            Unit.State := Registrar.Library_Units.Compiled;
            Registrar.Registration.Update_Library_Unit (Unit);
         end if;
      end;
      
   end Execute;
   
   -------------
   -- Compile --
   -------------
   
   procedure Compile (Configuration: in Build_Configuration) is
      use Registrar.Library_Units;
      
      New_Order: Compilation_Order 
        := (Tracker => Compilation_Progress'Access,
            Config  => Configuration,
            others  => <>);
      
      Avail_Units: constant Library_Unit_Sets.Set
        := Registrar.Queries.Available_Library_Units;
      
   begin
      
      -- Verify that we actually know where the compiler is before we dispatch
      -- a bunch of doomed work-orders. This will create one single obvious
      -- error rather than an unbounded number of hours if we don't do this
      -- here
      
      if not Program_Paths.Found (Compiler) then
         raise Program_Error with
           "Unable to compile: Could not find the compiler (" 
           & Compiler_Program & ")!";
      end if;
      
      Compilation_Progress.Increase_Total_Items_By
        (Natural (Avail_Units.Length));
      
      for Unit of Avail_Units loop
         New_Order.Unit := Unit;
         Workers.Enqueue_Order (New_Order);
      end loop;
      
   end Compile;
   
end Build.Compilation;
