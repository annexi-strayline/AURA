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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Directories;
with Ada.Command_Line;

with CLI; use CLI;
with UI_Primitives;
with Build, Build.Compilation, Build.Linking;
with Checkout;
with Progress;
with Workers.Reporting;
with Unit_Names, Unit_Names.Sets;
with Configuration;
with Repositories;
with Repositories.Cache;
with Registrar.Queries;
with Registrar.Subsystems;
with Registrar.Library_Units;
with Registrar.Registration;
with Registrar.Implementation_Hashing;
with Registrar.Dependency_Processing;
with Registrar.Last_Run_Store;

package body Scheduling is
   
   package UI  renames UI_Primitives;
   package UBS renames Ada.Strings.Unbounded;
   
   use type Ada.Containers.Count_Type;
   subtype Count_Type is Ada.Containers.Count_Type;
   
   -- Utilities
   
   function Trim (Source: in String; 
                  Side  : in Ada.Strings.Trim_End := Ada.Strings.Both)
                 return String
     renames Ada.Strings.Fixed.Trim;
   
   
   --
   -- Parameters
   --
   
   type Output_Style_Type   is (Normal, Verbose, Quiet);
   type Systemize_Mode_Type is (Show, Add);
   
   type Parameter_Set is
      record
         Last_Argument: Natural               := 0;
         -- The last argument read
         
         Output_Style  : Output_Style_Type    := Normal;
         Systemize_Mode: Systemize_Mode_Type  := Show;
         -- Set by -v and -q options
         
         Main_Unit     : Unit_Names.Unit_Name;
         -- Valid only for build/run commands. This is set via the
         -- the command line (if provided) and is assumed to be a UTF-8 string.
         
         Target_Units: Registrar.Library_Units.Library_Unit_Sets.Set;
         -- In the Bind phase, Target_Units will be initialized to either
         -- all Ada units, or to a singular Ada unit selecred via the value 
         -- of Main_Unit if not empty. The selected unit(s) are then passed to
         -- the binder. The resulting binder unit ("ada_main" for GNAT), is
         -- added to Target_Units.
         --
         -- In the Link_or_Archive phase, if Target_Units is a single unit,
         -- it is expanded to recursively include all dependencies. Otherwise,
         -- all external units are included. The expanted set is then used to
         -- link the final image, or to determine which objects are archived.
         
         Executable    : UBS.Unbounded_String;
         -- Valid only for run commands, This string is either
         -- set from the command line, or is computed based on the
         -- contents of Main_Unit. 
         --
         -- This value is set by the Link_Or_Archive phase
         --
         -- This string will be the full path to the executable.
         
         Build_Config  : Build.Build_Configuration;
         -- The build configuration passed into the Build package operations,
         -- also loaded and saved from the backing store
         
      end record;
   
   Parameters: Parameter_Set;
   
   ---------------------------
   -- Initialize_Parameters --
   ---------------------------
   
   procedure Initialize_Parameters is separate;
   
   --
   -- Processes
   --
   
   -----------
   -- Clean --
   -----------
   
   procedure Clean is
      use Ada.Directories;
      
      Config_Dir: constant String := Current_Directory & "/.aura";
   begin
      if Exists (Config_Dir)       then Delete_Tree (Config_Dir);       end if;
      if Exists (Build.Build_Root) then Delete_Tree (Build.Build_Root); end if;
   end Clean;
   
   ----------------
   -- Enter_Root --
   ----------------
   
   procedure Enter_Root is
      Process_Title: constant String := "Entering root items";
      
      Reg_Tracker: Progress.Progress_Tracker
        renames Registrar.Registration.Entry_Progress;
      
      Entered_Sys  : Count_Type;
      Entered_Units: Count_Type;
      
   begin
      UI.Prep_Tracker (Process_Title);
      Registrar.Registration.Enter_Root;
      UI.Wait_Tracker_Or_Abort (Process_Title => Process_Title,
                                Tracker       => Reg_Tracker);
      
      Entered_Sys   := Registrar.Queries.Available_Subsystems.Length;
      Entered_Units := Registrar.Queries.Entered_Library_Units.Length;

      Clear_Line;
      UI.Put_OK_Tag;
      Put_Line (Message => " Entered" 
                  &        Count_Type'Image (Entered_Sys)
                  &        (if Entered_Sys = 1 then
                               " root subsystem "
                            else
                               " root subsystems ")
                  &        '(' & Trim (Count_Type'Image (Entered_Units))
                  &        (if Entered_Units = 1 then
                               " unit)."
                            else
                               " units)."));
      
   end Enter_Root;
   
   -----------------------------
   -- Initialize_Repositories --
   -----------------------------
   
   procedure Initialize_Repositories is
      use Repositories;
      
      Process_Title: constant String := "Initializing repositories";
      Reg_Title    : constant String := "Waiting registration";
      
      Process_Tracker: Progress.Progress_Tracker
        renames Repositories.Initialize_Repositories_Tracker;
      
      Reg_Tracker: Progress.Progress_Tracker
        renames Registrar.Registration.Entry_Progress;
      
      Total_Repos: Repository_Count;
   begin
      Reg_Tracker.Reset;
      UI.Prep_Tracker (Process_Title => Process_Title,
                       Spinner_Only  => False);
      Repositories.Initialize_Repositories;
      UI.Wait_Tracker_Or_Abort (Process_Title => Process_Title,
                                Tracker       => Process_Tracker);
      UI.Wait_Tracker_Or_Abort (Process_Title => Reg_Title,
                                Tracker       => Reg_Tracker,
                                Spinner_Only  => True);
      
      Total_Repos := Total_Repositories;
      
      Clear_Line;
      UI.Put_OK_Tag;
      Put_Line (" Loaded" & Repository_Count'Image (Total_Repos)
                  & (if Total_Repos = 1 then
                        " repository."
                     else
                        " repositories."));
      
   end Initialize_Repositories;
   
   ----------------------------
   -- Add_Explicit_Checkouts --
   ----------------------------
   
   procedure Add_Explicit_Checkouts is
      use Ada.Command_Line;
      
      package WWU renames Ada.Strings.Wide_Wide_Unbounded;
      package UTF renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      package WWM renames Ada.Strings.Wide_Wide_Maps;
      
      function UTF_8 return Ada.Strings.UTF_Encoding.Encoding_Scheme
        renames Ada.Strings.UTF_Encoding.UTF_8;
      
      function To_UTF8 (Wide: WWU.Unbounded_Wide_Wide_String) return String is
      begin
         return UTF.Encode (Item          => WWU.To_Wide_Wide_String (Wide),
                            Output_Scheme => UTF_8,
                            Output_BOM    => False);
      end To_UTF8;
      
      Delimiters: constant WWM.Wide_Wide_Character_Set := WWM.To_Set (".%");
      Arg: WWU.Unbounded_Wide_Wide_String;
   begin
      if Parameters.Last_Argument = Argument_Count then return; end if;
      
      Clear_Line;
      for I in Parameters.Last_Argument .. Argument_Count loop
         WWU.Set_Unbounded_Wide_Wide_String 
           (Target => Arg,
            Source => UTF.Decode (Item         => Argument (I),
                                  Input_Scheme => UTF_8));
         
         -- Each argument should be a valid unit name, but should also be
         -- a subsystem name (contain no delimiters)
         
         if not Unit_Names.Valid_Unit_Name (WWU.To_Wide_Wide_String (Arg)) then
            UI.Put_Fail_Tag;
            Put_Line (" """ & To_UTF8 (Arg) 
                        & """ is not a valid subsystem name");
            raise Process_Failed;
            
         elsif WWU.Count (Source => Arg,
                          Set    => Delimiters) > 0
         then
            UI.Put_Fail_Tag;
            Put_Line (" Only subsystems can be checked out.");
            UI.Put_Empty_Tag;
            Put_Line (" """ & To_UTF8 (Arg) 
                        & """ is not a library-level unit.");
            raise Process_Failed;
         end if;
         
         -- Ask the Registrar to register it. If this fails, it means it is
         -- a root subsystem, and can't be requested
         
         declare
            OK: Boolean;
         begin
            Registrar.Registration.Request_AURA_Subsystem
              (Name => Unit_Names.Set_Name (WWU.To_Wide_Wide_String (Arg)),
               OK   => OK);
            
            if not OK then
               UI.Put_Fail_Tag;
               Put_Line (" Subsystem """ & To_UTF8 (Arg)
                           & """ is part of the root project,");
               UI.Put_Empty_Tag;
               Put_Line (" and can't be checked-out");
               raise Process_Failed;
            end if;
         end;
         
      end loop;
      
   end Add_Explicit_Checkouts;
   
   --------------------
   -- Checkout_Cycle --
   --------------------
   
   procedure Checkout_Cycle is
      
      Checkout_Tracker: Progress.Progress_Tracker
        renames Checkout.Checkout_Pass_Progress;
      Reg_Tracker: Progress.Progress_Tracker
        renames Registrar.Registration.Entry_Progress;
      Hash_Crunch_Tracker: Progress.Progress_Tracker
        renames Registrar.Implementation_Hashing.Crunch_Phase_Progress;
      Config_Tracker: Progress.Progress_Tracker
        renames Configuration.Configuration_Progress;
      Caching_Tracker: Progress.Progress_Tracker
        renames Repositories.Cache.Caching_Progress;

      Checkout_Title    : constant String := "Checking out subsystems";
      Reg_Title         : constant String := "Registering new units  ";
      Hash_Crunch_Title : constant String := "Hashing configurations ";
      Config_Title      : constant String := "Configuring subsystems ";
      Config_Root_Title : constant String := "Processing root config ";
      Cache_Title       : constant String := "Caching repositories   ";
      
      Pass: Positive := 1;
      Failures, Timedout: Boolean;
      Num_Requested: Count_Type;
      
   begin
      -- Check and dump any work reports that might exist. We don't expect
      -- any at this stage, but there's no harm in dumping them here, if they
      -- are part of debugging
      
      Clear_Line;
      
      if Workers.Reporting.Available_Reports > 0 then
         UI.Put_Info_Tag;
         Put_Line (" Unexpected worker reports. " 
                     & Count_Type'Image (Workers.Reporting.Available_Reports)
                     & " reports to follow:");
         UI.Dump_Reports;
      end if;
      
      
      loop
         Checkout_Tracker   .Reset;
         Reg_Tracker        .Reset;
         Hash_Crunch_Tracker.Reset;
         Config_Tracker     .Reset;
         Caching_Tracker    .Reset;
         
         -- Checkout requested repositories
         UI.Prep_Tracker (Checkout_Title);
         Checkout.Checkout_Pass;
         UI.Wait_Tracker (Process_Title => Checkout_Title,
                          Tracker       => Checkout_Tracker,
                          Failures      => Failures,
                          Timedout      => Timedout);
         
         if (Failures and then Workers.Reporting.Available_Reports > 0)
           or else Timedout 
         then
            -- This means something "uncommon" went wrong - something that was
            -- not the innocent failure to find a requested subsystem, and
            -- so we need to go through the more aggressive abort with worker
            -- reports route.
            
            -- Otherwise the failed items will result in a subsystem being
            -- set to the Unavailable state, which can be queried later
            
            raise Process_Failed;
         end if;
         
         UI.Wait_Tracker_Or_Abort (Process_Title => Reg_Title,
                                   Tracker       => Reg_Tracker);
         
         -- Hash any configuration or manifest units
         UI.Prep_Tracker (Hash_Crunch_Title);
         Registrar.Implementation_Hashing.Hash_Configurations;
         UI.Wait_Tracker_Or_Abort (Process_Title => Hash_Crunch_Title,
                                   Tracker       => Hash_Crunch_Tracker);
         
         -- Configuration run
         UI.Prep_Tracker (Config_Title);
         Configuration.Configuration_Pass;
         UI.Wait_Tracker_Or_Abort (Process_Title => Config_Title,
                                   Tracker       => Config_Tracker);
         
         -- Wait on entry of any condepaths
         UI.Wait_Tracker_Or_Abort (Process_Title => Reg_Title,
                                   Tracker       => Reg_Tracker);
         
         Num_Requested := Registrar.Queries.Requested_Subsystems.Length;
         Clear_Line;
         
         UI.Put_OK_Tag;
         UI.Put_Info (" Checkout - pass" & Positive'Image (Pass) 
                     & " completed. " 
                     & (if Num_Requested = 1 then
                           " 1 subsystem"
                        else
                           Count_Type'Image (Num_Requested)
                           & " subsystems") 
                     & " remaining.");
         
         exit when Num_Requested = 0;
         Pass := Pass + 1;
         
         -- Cache repos ahead of next run
         UI.Prep_Tracker (Cache_Title);
         Repositories.Cache.Cache_Repositories;
         UI.Wait_Tracker_Or_Abort (Process_Title   => Cache_Title,
                                   Tracker         => Caching_Tracker,
                                   Spinner_Only    => False,
                                   Process_Timeout => 120.0);
      end loop;
      
      -- Root config

      Config_Tracker.Reset;
      UI.Prep_Tracker (Config_Root_Title);
      Configuration.Process_Root_Configuration;
      UI.Wait_Tracker_Or_Abort (Process_Title => Config_Root_Title,
                                Tracker       => Config_Tracker);
      Clear_Line;
      
      -- Final tally
      declare
         use Registrar.Subsystems;
         
         All_Subsystems: constant Subsystem_Sets.Set
           := Registrar.Queries.All_Subsystems;
         
         Avail_Tally, Unavail_Tally: Count_Type := 0;
      begin
         for Subsys of All_Subsystems loop
            if Subsys.AURA then
               case Subsys.State is
                  when Available   => Avail_Tally   := Avail_Tally   + 1;
                  when Unavailable => Unavail_Tally := Unavail_Tally + 1;
                  when others      => null;
               end case;
            end if;
         end loop;
         
         if Unavail_Tally > 0 then
            UI.Put_Fail_Tag;
            Put_Line (" Failed to checkout" 
                        & Count_Type'Image (Unavail_Tally)
                        & " AURA " 
                        & (if Unavail_Tally = 1 then
                              "subsystem."
                           else
                              "subsystems.")
                        & " Details to follow..");
            
         else
            Registrar.Registration.Exclude_Manifests;
            
            UI.Put_OK_Tag;
            Put_Line (" Checked-out and configured"
                        & Count_Type'Image (Avail_Tally) 
                        & " AURA "
                        & (if Avail_Tally = 1 then 
                              "subsystem "
                           else
                              "subsystems ")
                        & "in"
                        & Positive'Image (Pass) &
                        (if Pass = 1 then
                            " pass."
                         else
                            " passes."));
         end if;
      end;
      
   end Checkout_Cycle;
   
   ------------------------------
   -- Consolidate_Dependencies --
   ------------------------------
   
   procedure Consolidate_Dependencies is
      Consolidation_Merge_Title  : constant String
        := "Consolidating dependencies (Merge)  ";
      Consolidation_Fan_Out_Title: constant String
        := "Consolidating dependencies (Fan-out)";
      Consolidation_Build_Title  : constant String
        := "Consolidating dependencies (Build)  ";
      
      Consolidation_Merge_Tracker: Progress.Progress_Tracker
        renames Registrar.Dependency_Processing.Merge_Subunits_Progress;
      Consolidation_Fan_Out_Tracker: Progress.Progress_Tracker
        renames Registrar.Dependency_Processing.Fan_Out_Progress; 
      Consolidation_Build_Tracker: Progress.Progress_Tracker
        renames Registrar.Dependency_Processing.Build_Reverse_Progress; 
   begin
      Consolidation_Merge_Tracker  .Reset;
      Consolidation_Fan_Out_Tracker.Reset;
      Consolidation_Build_Tracker  .Reset;
      
      UI.Prep_Tracker (Consolidation_Merge_Title);
      Registrar.Dependency_Processing.Consolidate_Dependencies;
      UI.Wait_Tracker_Or_Abort (Process_Title => Consolidation_Merge_Title,
                                Tracker       => Consolidation_Merge_Tracker);
      UI.Wait_Tracker_Or_Abort (Process_Title => Consolidation_Fan_Out_Title,
                                Tracker       => Consolidation_Fan_Out_Tracker);
      UI.Wait_Tracker_Or_Abort (Process_Title => Consolidation_Build_Title,
                                Tracker       => Consolidation_Build_Tracker);
      
      Clear_Line;
      UI.Put_OK_Tag;
      UI.Put_Info (" All dependency maps consolidated.");      
   end Consolidate_Dependencies;
   
   ----------------------
   -- Check_Completion --
   ----------------------
   
   procedure Report_Unavailable_Subsystems
     (Unavailable_Subsystems: in Registrar.Subsystems.Subsystem_Sets.Set)
   is separate;
   
   -- Dumps the information for the case when one or more AURA Subsystems
   -- could not be aquired
   
   procedure Report_Incomplete_Subsystems
     (Requested_Units: in Registrar.Library_Units.Library_Unit_Sets.Set)
   is separate;
   
   -- Dumps the information for the case when one or more units have not been
   -- entered
   
   --------------------------------------------------   
   procedure Check_Completion is
      use Registrar.Subsystems;
      use Registrar.Library_Units;
      
      package Subsystem_Unfulfilled_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Subsystem,
         Element_Type    => Library_Unit_Sets.Set,
         Hash            => Subsystem_Name_Hash,
         Equivalent_Keys => Equivalent_Subsystems,
         "="             => Library_Unit_Sets."=");
      
      Entered_Units: constant Library_Unit_Sets.Set
        := Registrar.Queries.Entered_Library_Units;
      
      Requested_Units: constant Library_Unit_Sets.Set
        := Registrar.Queries.Requested_Library_Units;
      
      Available_Subsystems: constant Subsystem_Sets.Set
        := Registrar.Queries.Available_Subsystems;
      
      Unavailable_Subsystems: constant Subsystem_Sets.Set
        := Registrar.Queries.Unavailable_Subsystems;
      
      pragma Assert (Registrar.Queries.Requested_Subsystems.Length = 0);
      pragma Assert (Registrar.Queries.Aquired_Subsystems.Length = 0);
      -- These conditions should be enforced by the Checkout_Cycle phase, which
      -- should either configure all aquired subsystems, or mark them as
      -- Unavailable
      
   begin
      Clear_Line;
      
      if Unavailable_Subsystems.Length > 0 then
         Report_Unavailable_Subsystems (Unavailable_Subsystems);
         raise Process_Failed;
         
      elsif Requested_Units.Length > 0 then
         -- If we have unavailable subsystems, we ignore missing units. Let's
         -- deal with one thing at a time
         UI.Put_Fail_Tag;
         Put_Line ("Some expected library units are missing.");
         UI.Put_Empty_Tag;
         Put_Line ("This may indicate subsystem version incompatabilities.");
         
         Report_Incomplete_Subsystems (Requested_Units);
         raise Process_Failed;
         
      else
         -- Everything looks good. Lets print some statistics
         UI.Put_OK_Tag;
         Put_Line (" All dependencies satisfied ("
                     & Trim(Count_Type'Image (Available_Subsystems.Length))
                     & (if Available_Subsystems.Length = 1 then
                           " subsystem,"
                        else
                           " subsystems,")
                     & Count_Type'Image (Entered_Units.Length)
                     & (if Entered_Units.Length = 1 then
                           " unit)."
                        else
                           " units)."));
         
      end if;
      
   end Check_Completion;
   
   -------------------
   -- Hash_Registry --
   -------------------
   
   procedure Hash_Registry is
      Hash_Collection_Title: constant String
        := "Hashing all units (Collection) ";
      Hash_Crunch_Title: constant String
        := "Hashing all units (Crunch)     ";
      Hash_Object_Title: constant String
        := "Hashing all units (Objects)    ";
      
      Hash_Collection_Tracker: Progress.Progress_Tracker
        renames Registrar.Implementation_Hashing.Collection_Phase_Progress;
      Hash_Crunch_Tracker: Progress.Progress_Tracker
        renames Registrar.Implementation_Hashing.Crunch_Phase_Progress;
      Hash_Object_Tracker: Progress.Progress_Tracker
        renames Build.Compilation_Hash_Progress;
      
   begin
      Clear_Line;
      
      Hash_Collection_Tracker.Reset;
      Hash_Crunch_Tracker    .Reset;
      Hash_Object_Tracker    .Reset;

      
      UI.Prep_Tracker (Hash_Collection_Title);
      Registrar.Implementation_Hashing.Hash_All;
      UI.Wait_Tracker_Or_Abort (Process_Title => Hash_Collection_Title,
                                Tracker       => Hash_Collection_Tracker);
      UI.Wait_Tracker_Or_Abort (Process_Title => Hash_Crunch_Title,
                                Tracker       => Hash_Crunch_Tracker);
      
      UI.Prep_Tracker (Hash_Object_Title);
      Build.Hash_Compilation_Products;
      UI.Wait_Tracker_Or_Abort (Process_Title => Hash_Object_Title,
                                Tracker       => Hash_Object_Tracker);
      
      Clear_Line;
      UI.Put_OK_Tag;
      UI.Put_Info (" All units hashed.");
      Clear_Line;
      
   end Hash_Registry;
   
   -------------
   -- Compile --
   -------------
   
   procedure Compile is
      use Registrar.Library_Units;
      
      Output_Quiet: constant Boolean := Parameters.Output_Style = Quiet;
      
      Compute_Recompilation_Title: constant String
        := "Computing Compilation Strategy ";
      Compile_Title: constant String
        := "Compiling ";
      
      Compute_Recompilation_Tracker: Progress.Progress_Tracker
        renames Build.Compute_Recompilations_Progress;
      Compile_Tracker: Progress.Progress_Tracker
        renames Build.Compilation.Compilation_Progress;
      
      Compile_Timeout : Boolean;
      Compile_Failures: Boolean;
      
      Pending_Compile: Count_Type;
      
   begin
      Clear_Line;
      
      if Workers.Reporting.Available_Reports > 0 then
         UI.Put_Info_Tag;
         Put_Line (" Unexpected worker reports. " 
                     & Count_Type'Image (Workers.Reporting.Available_Reports)
                     & " reports to follow:");
         UI.Dump_Reports;
      end if;
      
      Compute_Recompilation_Tracker.Reset;
      Compile_Tracker.Reset;
      
      UI.Prep_Tracker (Compute_Recompilation_Title);
      Build.Compute_Recompilations (Parameters.Build_Config);
      UI.Wait_Tracker_Or_Abort (Process_Title => Compute_Recompilation_Title,
                                Tracker       => Compute_Recompilation_Tracker);
      
      Clear_Line;
      UI.Put_Exec_Tag;
      Put (' ' & Compute_Recompilation_Title & "(Updating registry...)");
      Build.Compute_Recompilations_Completion.Wait_Leave;
      
      Pending_Compile := Registrar.Queries.Available_Library_Units.Length;
        
      Clear_Line;
      UI.Put_OK_Tag;
      
      if Pending_Compile = 0 then
         -- This should mean that no recompilations are necessary.
         pragma Assert 
           (for all Unit of Registrar.Queries.All_Library_Units
              => Unit.State = Compiled 
                or else (Unit.Kind = Subunit and Unit.State = Available));
         

         Put_Line (" No units required compilation.");
         return;
      else
         Put_Line (Count_Type'Image (Pending_Compile)
                     & (if Pending_Compile = 1 then
                           " unit"
                        else
                           " units")
                     & " pending compilation..");
      end if;
      
      UI.Prep_Tracker (Compile_Title);
      Build.Init_Paths;
      Build.Compilation.Compile (Parameters.Build_Config);
      UI.Wait_Tracker (Process_Title => Compile_Title,
                       Tracker       => Compile_Tracker,
                       Failures      => Compile_Failures,
                       Timedout      => Compile_Timeout);
      
      
      if Compile_Timeout then
         UI.Put_Fail_Tag;
         Put_Line (" Compilation timed-out.");
      end if;
      
      Clear_Line;
      
      if Build.Compilation.Compiler_Messages.Current_Use > 0 then
         if Compile_Failures then
            UI.Put_Fail_Tag;
            Put_Line (Natural'Image (Compile_Tracker.Failed_Items) 
                        & " of"  & Natural'Image (Compile_Tracker.Total_Items)
                        & " units failed to compile:");

         else
            UI.Put_Warn_Tag;
            Put_Line (Natural'Image (Compile_Tracker.Completed_Items)
                        & " units compiled successfully."
                        & Count_Type'Image
                          (Build.Compilation.Compiler_Messages.Current_Use)
                        & " units had warnings"
                        & (if Output_Quiet then '.' else ':'));
         end if;
         
         -- Now display the messages for each unit
         if Output_Quiet then
            UI.Put_Info_Tag;
            Put_Line (" Compiler output suppressed. ");
            Ui.Put_Empty_Tag;
            Put_Line (" Refer to contents of aura-build/build_output/");
         end if;
         
         declare
            use Build.Compilation;
            
            Message: Compiler_Message;
         begin
            loop
               select
                  Compiler_Messages.Dequeue (Message);
               else
                  exit;
               end select;
               
               case Message.Context is
                  when Warning =>
                     UI.Put_Warn_Tag;
                     Put_Line (' ' & Message.Unit.Name.To_UTF8_String
                                 & ": Compilation completed with warnings" 
                                 & (if Output_Quiet then '.' else ':')); 
                  when Error =>
                     UI.Put_Fail_Tag;
                     Put_Line (' ' & Message.Unit.Name.To_UTF8_String
                                 & ": Compilation failed"
                                 & (if Output_Quiet then '.' else ':'));
               end case;
               
               if not Output_Quiet then
                  Put_Line (UBS.To_String (Message.STDERR));
               end if;
            end loop;
         end;
         
         if Compile_Failures or Compile_Timeout then
            raise Build_Failed;
         end if;
         
      elsif Compile_Timeout then
         raise Build_Failed;
         
      else
         
         UI.Put_OK_Tag;
         Put_Line (Natural'Image (Compile_Tracker.Completed_Items)
                     & " units compiled successfully.");
         
         -- All units should now be compiled. Note that if we have an actual
         -- Library_Unit of Kind "Subunit", it means it is an Ada subunit that
         -- has its own subunits, and therefore, like all subunits, cannot
         -- actually be compiled

         pragma Assert 
           (for all Unit of Registrar.Queries.All_Library_Units
              => Unit.State = Compiled 
                or else (Unit.Kind = Subunit and Unit.State = Available));
      end if;
      
      
   end Compile;
   
   ----------
   -- Bind --
   ----------
   
   procedure Bind is 
      use Registrar.Library_Units;
      
      Binding_Title: constant String := " Binding...";
   begin
      Clear_Line;
      UI.Put_Exec_Tag;
      Put (Binding_Title);
      
      -- Set-up the Parameters.Target_Units depending on if we have a
      -- "main unit" or not
      
      if not Parameters.Main_Unit.Empty then
         if not Registrar.Queries.Unit_Registered (Parameters.Main_Unit) then
            Clear_Line;
            UI.Put_Fail_Tag;
            Put_Line (" Binding failed. Main unit """ 
                        &  Parameters.Main_Unit.To_UTF8_String
                        & """ does not exist.");
            raise Build_Failed;
         end if;
         
         declare
            Main_Unit: constant Library_Unit
                 := Registrar.Queries.Lookup_Unit (Parameters.Main_Unit);
         begin
            case Main_Unit.Kind is
               when Package_Unit | Subprogram_Unit =>
                  Parameters.Target_Units
                    := Library_Unit_Sets.To_Set (Main_Unit);
                  
               when External_Unit =>
                  -- AURA does not support using External_Units as the main
                  -- unit. The proper way to do this is to create a library.
                  
                  Clear_Line;
                  UI.Put_Fail_Tag;
                  Put_Line (" The main unit must be an Ada unit."
                              & " Please consider the 'library' command.");
                  raise Build_Failed;
                  
               when Subunit =>
                  Clear_Line;
                  UI.Put_Fail_Tag;
                  Put_Line (" The main unit cannot be a Subunit.");
                  raise Build_Failed;
                  
               when Unknown =>
                  Clear_Line;
                  UI.Put_Fail_Tag;
                  Put_Line (" Internal error: main unit is of Kind 'Unknown'.");
                  raise Build_Failed;
            end case;
         end;
         
      else
         -- All (Ada) units are bound
         Parameters.Target_Units := Registrar.Queries.Ada_Library_Units;
      end if;
      
      pragma Assert (for all Unit of Parameters.Target_Units => 
                       Unit.State = Compiled
                       and Unit.Kind in Package_Unit | Subprogram_Unit);
      
      declare
         use UBS;
         Bind_Errors: Unbounded_String;
      begin
         Build.Linking.Bind (Unit_Set      => Parameters.Target_Units,
                             Configuration => Parameters.Build_Config,
                             Errors        => Bind_Errors);
         
         Clear_Line;
         
         if Length (Bind_Errors) > 0 then
            UI.Put_Fail_Tag;
            Put_Line (" Binding failed:");
            New_Line;
            Put_Line (To_String (Bind_Errors));
            raise Build_Failed;
         end if;
         
      exception
         when Build_Failed =>
            raise;
            
         when e: others =>
            Clear_Line;
            UI.Put_Fail_Tag;
            Put_Line (" Bind failed with an unexpected exception");
            Put_Line (" -- Binder errors --");
            Put_Line (To_String (Bind_Errors));
            Put_Line (" -- Exception Information --");
            Put_Line (Ada.Exceptions.Exception_Information (e));
            raise Build_Failed;
            
            -- Since Binding is always done "fresh", it's safe to always
            -- raise a Build_Failed in this case
      end;
      
      UI.Wait_Tracker_Or_Abort 
        (Process_Title => " Entering binder unit",
         Tracker       => Registrar.Registration.Entry_Progress,
         Spinner_Only  => True);
      
      -- Interesting fact here - since we entered this binder unit after
      -- we did the recompilation check orders, it will never be subject
      -- to that check, and thus will always be compiled. This is
      -- intentional since this entire binder process is GNAT-specific,
      -- and AURA is designed to be as compiler-agnostic as possible.
      --
      -- This is a very small price to pay.
      
      Clear_Line;
      Build.Compilation.Compilation_Progress.Reset;
      Build.Compilation.Compile (Parameters.Build_Config);
      UI.Wait_Tracker_Or_Abort 
        (Process_Title => " Compiling binder unit",
         Tracker       => Build.Compilation.Compilation_Progress,
         Spinner_Only  => True);
      
      -- Incorporate the binder unit into Target_Units
      declare
         use Unit_Names;
         Binder_Unit: constant Library_Unit
           := Registrar.Queries.Lookup_Unit (Set_Name ("ada_main"));
      begin
         Parameters.Target_Units.Include (Binder_Unit);
      exception
         when e: others =>
            Clear_Line;
            UI.Put_Fail_Tag;
            Put_Line (" Binder unit could not be included");
            Put_Line (" -- Exception Information --");
            Put_Line (Ada.Exceptions.Exception_Information (e));
            raise Process_Failed;
      end;
      
      Clear_Line;
      UI.Put_OK_Tag;
      Put_Line (" Binding complete.");
      
   end Bind;
   
   -------------------------
   -- Expand_Dependencies --
   -------------------------
   
   procedure Expand_Dependencies is
      procedure Recursive_Add_Dependencies 
        (Unit: Registrar.Library_Units.Library_Unit);
      
      procedure Recursive_Add_Dependencies 
        (Unit: Registrar.Library_Units.Library_Unit)
      is 
         use Registrar.Library_Units;
         
         New_Adds: Library_Unit_Sets.Set 
           := Registrar.Queries.Unit_Dependencies (Unit);
      begin
         New_Adds.Difference (Parameters.Target_Units);
         Parameters.Target_Units.Union (New_Adds);
         
         -- This approach ensures that cycles are not possible if the codebase
         -- has circular dependencies (legal with limited withs), since we only
         -- ever do a recursive call on units that do not already exist in the
         -- Target_Units, and we always add those units to the target set
         -- before recursing.
         
         for New_Unit of New_Adds loop
            Recursive_Add_Dependencies (New_Unit);
         end loop;
      end Recursive_Add_Dependencies;
      
      Op_Name: constant String := " Expanding Main Unit Dependencies...";
   begin
      if Parameters.Main_Unit.Empty then 
         -- In this case we're building "everything", so just add in any
         -- external units
         Parameters.Target_Units.Union (Registrar.Queries.External_Units);
         return; 
      end if;
      
      Clear_Line;
      UI.Put_Exec_Tag;
      Put (Op_Name);
      
      -- We are going to be tampering with Parameters.Target_Unit, so we
      -- need to make a copy. The existing set should only contain two
      -- units.
      
      declare
         use Registrar.Library_Units;
         
         Expansion_Base: constant Library_Unit_Sets.Set
           := Parameters.Target_Units;
      begin
         for Base_Unit of Expansion_Base loop
            Recursive_Add_Dependencies (Base_Unit);
         end loop;
      end;
      
      Clear_Line;
      UI.Put_OK_Tag;
      Put_Line (Op_Name & " Done.");
   end Expand_Dependencies;
   
   -------------------------
   -- Scan_Linker_Options --
   -------------------------
   
   procedure Scan_Linker_Options is
      Process_Title: constant String := "Scaning linker options";
      Tracker: Progress.Progress_Tracker 
        renames Build.Linking.Scan_Progress;
   begin
      UI.Prep_Tracker (Process_Title);
      Tracker.Reset;
      Build.Linking.Scan_Linker_Options (Registrar.Queries.Ada_Library_Units);
      UI.Wait_Tracker_Or_Abort (Process_Title => Process_Title,
                                Tracker       => Tracker);
      Clear_Line;
      
      
      UI.Put_OK_Tag;
      Put_Line 
        (" Scanned" & Natural'Image (Tracker.Completed_Items) 
           & (if Tracker.Completed_Items = 1 then 
                 " unit "
              else
                 " units ")
           & "finding" 
           & Count_Type'Image (Build.Linking.Linker_Options.Current_Use)
           & (if Build.Linking.Linker_Options.Current_Use = 1 then
                 " linker option."
              else
                 " linker options."));
      
   end Scan_Linker_Options;
   
   ---------------------
   -- Link_Or_Archive --
   ---------------------
   
   procedure Link_Or_Archive is
      use UBS;
      
      Link_Errors: Unbounded_String;
      

   begin
      Clear_Line;
      UI.Put_Exec_Tag;
      
      case Command is
         when Build_Command | Run_Command =>
            Put (" Linking Image...");
            
            declare
               Image_Name: constant String
                 := (if Parameters.Main_Unit.Empty then
                        "aura.out"
                     else
                        Parameters.Main_Unit.To_UTF8_String);
               
               Image_Path: constant String
                 := Ada.Directories.Current_Directory & '/' & Image_Name;
               
            begin
               if Command = Run_Command then
                  UBS.Set_Unbounded_String (Target => Parameters.Executable,
                                            Source => Image_Path);
               end if;
               
               Build.Linking.Link_Image
                 (Image_Path    => Image_Path,
                  Unit_Set      => Parameters.Target_Units,
                  Configuration => Parameters.Build_Config,
                  Errors        => Link_Errors);
            end;
            
         when Library_Command =>
            Put (" Linking Library..");
            
            declare
               -- The argument parser (Initialize_Parameters) ensures that
               -- there is a valid library name following the "last argument"
               -- in this case
               
               Library_Name: constant String 
                 := Ada.Command_Line.Argument (Parameters.Last_Argument + 1);
               
               Library_Extension: constant String 
                 := Ada.Directories.Extension (Library_Name);
               
               Library_Path: constant String
                 := Ada.Directories.Current_Directory & '/' & Library_Name;
            begin
               if Library_Extension in "a" | "A" then
                  Build.Linking.Archive
                    (Archive_Path  => Library_Path,
                     Unit_Set      => Parameters.Target_Units,
                     Configuration => Parameters.Build_Config,
                     Errors        => Link_Errors);
                  
               else
                  -- Assume a shared library
                  Build.Linking.Link_Library
                    (Library_Path  => Library_Path,
                     Unit_Set      => Parameters.Target_Units,
                     Configuration => Parameters.Build_Config,
                     Errors        => Link_Errors);
               end if;
            end;
            
            
         when others =>
            -- Failed precondition
            raise Program_Error with
              "Must be invoked when Command is Build, Run, or Library.";
      end case;
      
      Clear_Line;
      
      UI.Put_OK_Tag;
      Put_Line (" Link complete.");
      
      if Length (Link_Errors) > 0 then
         UI.Put_Fail_Tag;
         Put_Line (" Linking failed:");
         New_Line;
         Put_Line (To_String (Link_Errors));
         raise Process_Failed;
      end if;
      
   exception
      when Process_Failed => 
         raise;
         
      when e: others =>
         Clear_Line;
         UI.Put_Fail_Tag;
         Put_Line (" Linker failed with an unexpected exception");
         Put_Line (" -- Linker errors --");
         Put_Line (To_String (Link_Errors));
         Put_Line (" -- Exception Information --");
         Put_Line (Ada.Exceptions.Exception_Information (e));
         raise Process_Failed;
         
   end Link_Or_Archive;
   
   -------------------
   -- Execute_Image --
   -------------------
   
   procedure Execute_Image is separate;
   
   -------------------
   -- Save_Registry --
   -------------------
   
   procedure Save_Registry is
   begin
      Registrar.Last_Run_Store.Store_Current_Run;
   exception
      when e: others =>
         -- This makes the project inconsistent
         New_Line;
         
         UI.Put_Warn_Tag;
         Put_Line (" Unable to save registry.");
         
         UI.Put_Warn_Tag;
         Put_Line (' ' & Ada.Exceptions.Exception_Information (e));
         
         UI.Put_Warn_Tag;
         Put_Line (" Invoke aura clean before next run.");
   end Save_Registry;
   
   -----------------
   -- Save_Config --
   -----------------
   
   procedure Save_Config is
   begin
      Build.Store_Build_Config (Parameters.Build_Config);
      
   exception
      when e: others =>
         New_Line;
         
         UI.Put_Warn_Tag;
         Put_Line (" Unable to save configuration");
         
         UI.Put_Warn_Tag;
         Put_Line (' ' & Ada.Exceptions.Exception_Information (e));
         
         UI.Put_Warn_Tag;
         Put_Line (" Invoke aura clean before next run.");
   end Save_Config;
   
end Scheduling;
