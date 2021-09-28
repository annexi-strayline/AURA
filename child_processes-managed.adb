------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
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

with Ada.Unchecked_Deallocation;

package body Child_Processes.Managed is
   
   -----------------------
   -- Termination_Watch --
   -----------------------
   
   task body Termination_Watch is
      Exit_Code: Exit_Status;
   begin
      -- Note that this task is always activated after Proc.PID has been
      -- set. This can happen either through an allocator, or once GNAT
      -- if fixed, through the activation rules of tasks in objects returned
      -- from a function (Span_Process)
      
      Platform.Wait_Termination (ID        => Proc.PID,
                                 Exit_Code => Exit_Code);
      
      loop
         select
         accept Wait_Terminate (Status: out Exit_Status) do
               Status := Exit_Code;
            end;
         or
            terminate;
         end select;
      end loop;
   end Termination_Watch;
   
   ---------------
   -- IO_Stream --
   ---------------
   
   function IO_Stream (Process : in out Managed_Process;
                       Selector: in     IO_Stream_Selector) 
                      return not null access Root_Stream_Type'Class
     is (Process.Streams(Selector)'Unchecked_Access);
     
   ------------------------
   -- Set_Stream_Timeout --
   ------------------------
   
   procedure Set_Stream_Timeout (Process  : in out Managed_Process;
                                 Selector : in     IO_Stream_Selector;
                                 Timeout  : in     Duration)
   is begin
      Process.Streams(Selector).Timeout := Timeout;
   end Set_Stream_Timeout;
   
   ----------------
   -- Terminated --
   ----------------
   
   function Terminated (Process: Managed_Process) 
                       return Boolean
   is 
      Discard: Exit_Status;
   begin
      select
         Process.Watch.Wait_Terminate (Discard);
         return True;
      else
         return False;
      end select;
   end Terminated;
   
   ---------------------
   -- Wait_Terminated --
   ---------------------

   procedure Wait_Terminated (Process  : in     Managed_Process;
                              Timeout  : in     Duration;
                              Timed_Out:    out Boolean;
                              Status   :    out Exit_Status)
   is begin
      if Timeout = 0.0 then
         Timed_Out := False;
         Process.Watch.Wait_Terminate (Status);
         
      else
         
         select
            Process.Watch.Wait_Terminate (Status);
            Timed_Out := False;
            return;
         or
            delay Timeout;
            Timed_Out := True;
            return;
         end select;
         
      end if;
   end Wait_Terminated;
   
   ----------
   -- Kill --
   ----------
   
   procedure Kill (Process: in out Managed_Process) is
   begin
      Platform.Kill (Process.PID);
   end Kill;
   
   ----------
   -- Nuke --
   ----------
   
   procedure Nuke (Process: in out Managed_Process) is
   begin
      Platform.Nuke (Process.PID);
   end Nuke;
   
   --------------
   -- Finalize --
   --------------
   
   overriding
   procedure Finalize (Process: in out Managed_Process) is
      procedure Free_Watch is new Ada.Unchecked_Deallocation
        (Object => Termination_Watch,
         Name   => Termination_Watch_Access);
   begin
      for Stream of Process.Streams loop
         Platform.Close_Stream (Stream.Handle);
      end loop;
      
      if not Process.Terminated then
         -- No time to mess around
         Platform.Nuke (Process.PID);
      end if;
      
      Free_Watch (Process.Watch);
      
   end Finalize;
   
end Child_Processes.Managed;
