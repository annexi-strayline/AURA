------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2023, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- This is the "driver" (main program). It essentially handles sequencing
-- of the Scheduling package operations, and provides a final exception handler

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with System.Multiprocessors;

with CLI; use CLI;

with Workers, Workers.Reporting;
with Registrar.Queries;
with Registrar.Library_Units;
with Unit_Names;
with UI_Primitives; use UI_Primitives;
with Scheduling;



procedure AURA is
   pragma Linker_Options ("tty_ifo.o");
   -- This is required for building AURA CLI, since it relies on the ASAP
   -- repository's CLI AURA subsystem. If using CLI with AURA, AURA will ensure
   -- this "External Unit" is linked into the final program. However, AURA CLI
   -- is built without AURA, and so we need to include this so that gnatmake
   -- knows that it needs to include this object file.
   
   use CLI;
   use type Workers.Count_Type;
   use type Workers.Worker_Count;
   use type System.Multiprocessors.CPU_Range;
   use type Ada.Exceptions.Exception_ID;
   
   use all type Scheduling.Selected_Command;
   
   package UBS renames Ada.Strings.Unbounded;
   
   Base_Workers: constant Workers.Worker_Count
     := Workers.Worker_Count (System.Multiprocessors.Number_Of_CPUs);
   
   Optimal_Workers: constant Workers.Worker_Count
     := (if Base_Workers > 1 then Base_Workers + 2
         else Base_Workers);
   
   Task_Group: Workers.Worker_Pool (1 .. Optimal_Workers);
   
   Command: Scheduling.Selected_Command renames Scheduling.Command;
   
   procedure Annul_Zombies (Timeout: Duration := 2.0) is
      Timedout: Boolean := False;
   begin
      -- Ask workers that don't disband in 2 seconds are probably in dire shape.
      -- Take them out of their misery so that we can all get out of here.
      
      if Timeout > 0.0 then
         Workers.Wait_Workers (Timeout => Timeout, Timedout => Timedout);
      end if;
      
      if Timedout then
         for T of Task_Group loop
            abort T;
         end loop;
      end if;
   end Annul_Zombies;
   
   
   procedure Complete is
      Timedout: Boolean;
   begin
      Workers.Disband_Workers;
      Workers.Wait_Workers (Timeout => 2.0, Timedout => Timedout);
      New_Line;
      
      if Timedout then
         Put_Warn_Tag;
         Put_Line (" Some workers failed to complete.");
         Annul_Zombies (Timeout => 0.0);
         
      else
         Put_OK_Tag;
         Put_Line (Message => " Command completed successfully.",
                   Style   => Bold);
      end if;

      if Workers.Reporting.Available_Reports > 0 then
         New_Line;
         Put_Warn_Tag; 
         Put_Line (" Unexpected worker reports:");
         UI_Primitives.Dump_Reports;
         Put_Line ("-- End of Worker Reports --");
      end if;
   end;
   
begin
   
   UI_Primitives.Print_Banner;
   Scheduling.Initialize_Parameters;
   
   case Command is
      when Help_Command =>
         UI_Primitives.Print_Help;
         Workers.Disband_Workers;
         return;
         -- We return because we don't want to show
         -- "Command completed successfully"
         
      when Clean_Command =>
         Scheduling.Clean;
         
         Complete;
         return;
         
      when Checkout_Command =>
         Scheduling.Enter_Project;
         Scheduling.Add_Explicit_Checkouts;
         Scheduling.Initialize_Repositories;
         Scheduling.Checkout_Cycle;
         Scheduling.Consolidate_Dependencies;
         Scheduling.Check_Completion;
         
         Complete;
         return;
         
      when Compile_Command =>
         Scheduling.Enter_Project;
         Scheduling.Initialize_Repositories;
         Scheduling.Checkout_Cycle;
         Scheduling.Consolidate_Dependencies;
         Scheduling.Check_Completion;
         Scheduling.Hash_Registry;
         Scheduling.Compile;
         Scheduling.Hash_Registry;
         Scheduling.Save_Registry;
         Scheduling.Save_Config;
         
         Complete;
         return;
         
      when Build_Command =>
         Scheduling.Enter_Project;
         Scheduling.Initialize_Repositories;
         Scheduling.Checkout_Cycle;
         Scheduling.Consolidate_Dependencies;
         Scheduling.Check_Completion;
         Scheduling.Hash_Registry;
         Scheduling.Compile;
         Scheduling.Bind;
         Scheduling.Hash_Registry;
         Scheduling.Expand_Dependencies;
         Scheduling.Scan_Linker_Options;
         Scheduling.Link_Or_Archive;
         Scheduling.Save_Registry;
         Scheduling.Save_Config;
         
         Complete;
         return;
         
      when Run_Command =>
         Scheduling.Enter_Project;
         Scheduling.Initialize_Repositories;
         Scheduling.Checkout_Cycle;
         Scheduling.Consolidate_Dependencies;
         Scheduling.Check_Completion;
         Scheduling.Hash_Registry;
         Scheduling.Compile;
         Scheduling.Bind;
         Scheduling.Hash_Registry;
         Scheduling.Expand_Dependencies;
         Scheduling.Scan_Linker_Options;
         Scheduling.Link_Or_Archive;
         Scheduling.Save_Registry;
         Scheduling.Save_Config;
         
         Complete;
         Scheduling.Execute_Image; -- No_Return;
         return; -- Unreachable
         
      when Library_Command =>
         Scheduling.Enter_Project;
         Scheduling.Initialize_Repositories;
         Scheduling.Checkout_Cycle;
         Scheduling.Consolidate_Dependencies;
         Scheduling.Check_Completion;
         Scheduling.Hash_Registry;
         Scheduling.Compile;
         Scheduling.Bind;
         Scheduling.Hash_Registry;
         Scheduling.Scan_Linker_Options;
         Scheduling.Link_Or_Archive;
         Scheduling.Save_Registry;
         Scheduling.Save_Config;
         
         Complete;
         return;
         
      when Systemize_Command =>
         raise Scheduling.Process_Failed with
           "Systemize is not yet implemented.";
         
   end case;

   
exception
   when e: Scheduling.Process_Failed | Scheduling.Build_Failed =>
      
      if Ada.Exceptions.Exception_Identity (e) 
        = Scheduling.Build_Failed'Identity 
      then
         -- Build_Failed means that AURA itself (per se) didn't have any
         -- problems. This means we can safely take note of what units did
         -- compile ok, and save that registry for next time.
         --
         -- Most cases where this triggers, there is an error in the user's
         -- code that caused compilation to fail. Therefore we want to do
         -- our best to ensure that the next run only needs to attempt to
         -- compile those units that failed, rather than everything.
         
         New_Line;
         Put_Info_Tag;
         Put_Line (" Saving registry for next time...");
         
         -- Safe to save the registry
         Scheduling.Hash_Registry;
         Scheduling.Save_Registry;
         Scheduling.Save_Config;
         Clear_Line;
      end if;
      
      New_Line;
      Put (Message => " AURA Abort ", Style => Red_BG + White_FG);
      
      Workers.Disband_Workers;

      
      if Workers.Reporting.Available_Reports = 0 then
         Put_Line (" Command canceled due to failed conditions");
         
      else
         Put_Line (Message => Workers.Count_Type'Image
                     (Workers.Reporting.Available_Reports)
                     & " Worker "
                     & (if Workers.Reporting.Available_Reports = 1 then
                           "Report "
                        else
                           "Reports")
                     & " to follow:",
                   Style => Bold);
         New_Line;
         UI_Primitives.Dump_Reports;
         Put_Line ("-- End of Worker Reports --");
         New_Line;
      end if;
      


      Annul_Zombies;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      
   when e: others =>
      Workers.Disband_Workers;
      New_Line;
      Put (Message => " Unexpected Exception ", Style => Red_BG + White_FG);
      Put_Line (" The driver failed unexpectedly:");
      Put_Line (Ada.Exceptions.Exception_Information (e));
      
      Annul_Zombies;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end AURA;
