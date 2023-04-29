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

-- This package contains routines and utilities for driving the CLI.
--
-- Note that these primatives also react to non-terminal executions, and omit
-- fancier UI features in those circumstances

with Ada.Text_IO;

with CLI;
with Progress;

package UI_Primitives is
   
   -- Configuration
   Progress_Poll_Rate: constant Duration := 0.05;
   
   Auto_Queries: Boolean := False;
   -- If True on entry to User_Query, the Default Response is immediately
   -- submitted
   
   -- Standard Error Routines
   package STDERR is
      
      package TIO renames Ada.Text_IO;
      
      procedure Put (File: in TIO.File_Type := TIO.Standard_Error;
                     Item: in String)
        renames TIO.Put;
      
      procedure Put_Line (File   : in TIO.File_Type := TIO.Standard_Error;
                          Message: in String)
        renames TIO.Put_Line;
      
      procedure New_Line (File   : in TIO.File_Type      := TIO.Standard_Error;
                          Spacing: in TIO.Positive_Count := 1)
        renames TIO.New_Line;
   end STDERR;
   
   
   -- Static content   
   procedure Print_Banner;
   procedure Print_Help;
   
   -- Line tags
   type Tag_Type is (EXEC, OK, FAIL, WARN, QUERY, INFO);
   procedure Put_Tag (Tag: Tag_Type);
   
   procedure Put_Exec_Tag  (Tag: Tag_Type := EXEC ) renames Put_Tag;
   procedure Put_OK_Tag    (Tag: Tag_Type := OK   ) renames Put_Tag;
   procedure Put_Fail_Tag  (Tag: Tag_Type := FAIL ) renames Put_Tag;
   procedure Put_Warn_Tag  (Tag: Tag_Type := WARN ) renames Put_Tag;
   procedure Put_Query_Tag (Tag: Tag_Type := QUERY) renames Put_Tag;
   procedure Put_Info_Tag  (Tag: Tag_Type := INFO ) renames Put_Tag;
   
   procedure Put_Empty_Tag; 
   -- Writes a series of spaces the same length as a tag
   
   procedure Put_Divider;
   -- Outputs a full width string of '-' characters

   procedure Query_Driver (Prompt  : in     String;
                           Default : in     String;
                           Response:    out String;
                           Last    :    out Natural)
   with Pre => Default'Length = Response'Length;
   -- For use with the User_Queries package
   
   procedure Immediate_YN_Query (Prompt  : in     String;
                                 Default : in     Boolean;
                                 Response:    out Boolean);
   
   -- For use outside of Worker Tasks (such as Scheduling) to initiate an
   -- immediate Yes/No query via the Query subsystem and the above Query_Driver
   -- implementation.
   --
   -- " (y/n)" will be appended to Prompt.
   --
   -- Should not be used if any Worker Tasks are active.
   
   procedure Dump_Reports;
   
   -- Debug
   procedure Dump_Repositories;
   procedure Dump_Subsystems;
   procedure Dump_Library_Units;
   
   -- Normal Operations
   procedure Prep_Tracker (Process_Title: in String;
                           Spinner_Only : in Boolean := False);
   -- Clears the current line and outputs Process_Title and a dummy progress
   -- bar in the style of Wait_Tracker. This is used before a sequential action
   -- that dispatches work orders is executed, particularily when that action
   -- may take a noticable amount of time.
   --
   -- Prep_Tracker has no effect if output is not to a terminal
   
   procedure Wait_Tracker (Process_Title  : in     String;
                           Tracker        : in out Progress.Progress_Tracker;
                           Failures       :    out Boolean;
                           Timedout       :    out Boolean;
                           Spinner_Only   : in     Boolean  := False;
                           Process_Timeout: in     Duration := 60.0);
   
   -- Clears the current line and outputs Process_Title, a progress bar, as
   -- well as a a numeric completed/total value.
   --
   -- User Queries are handled until the tracker completes.
   --
   -- If Spinner_Only is true, the progress bar and numeric counters are not
   -- displayed.
   --
   -- This information is updated at Progress_Pool_Rate, until the progress
   -- tracker completes, or if Progress_Total_Timeout elapses without any
   -- changes to the tracker values
   
   procedure Wait_Tracker_Or_Abort 
     (Process_Title  : in     String;
      Tracker        : in out Progress.Progress_Tracker;
      Spinner_Only   : in     Boolean  := False;
      Process_Timeout: in     Duration := 20.0);
   
   -- Invokes Wait_Tracker, and reports Timeout or Failed, before raising
   -- the Scheduling.Process_Failed exception
   
   
   procedure Put_Info (Message: String);
   
   -- If the output is a Terminal, this acts as a Put,
   -- If the output is not a Terminal, this acts as a Put_Line.
   --
   -- The purpose of this operation is to make interactive runs a bit less
   -- chatty, while scripted or CI/CD retain full output
   
end UI_Primitives;
