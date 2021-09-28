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

with Ada.Finalization; use Ada.Finalization;
 
with Child_Processes.Standard_IO; use Child_Processes.Standard_IO;
with Child_Processes.Platform;

private package Child_Processes.Managed is
   
   type Managed_Process;
   task type Termination_Watch (Proc: not null access Managed_Process) is
      entry Wait_Terminate (Status: out Exit_Status);
   end Termination_Watch;
   
   type Termination_Watch_Access is access Termination_Watch;


   -- Note that we don't make Termination_Watch an actual component of
   -- Managed_Process yet do to a GNAT bug (we have submitted the patch).
   --
   -- Once the patch has made it into a later stage mainstream GCC (likely
   -- GCC 10), we can reverse this work-around
   
   -- Termination watch waits on the process. Once the process
   -- terminates, Wait_Terminate will be serviced immediately, and perpetually
   -- until the containing Managed_Process is finalized.
   
   type Stream_Set is array (IO_Stream_Selector) of aliased Standard_IO_Stream;
   
   type Managed_Process is new Limited_Controlled and Child_Process with
      record
         PID: Platform.Process_ID;
         Watch: Termination_Watch_Access;
         
         -- Standard IO (from the perspective of the process
         Streams: Stream_Set;
      end record;
   
   overriding
   function IO_Stream (Process : in out Managed_Process;
                       Selector: in     IO_Stream_Selector) 
                      return not null access Root_Stream_Type'Class;
   
   overriding
   procedure Set_Stream_Timeout (Process : in out Managed_Process;
                                 Selector: in     IO_Stream_Selector;
                                 Timeout : in     Duration);
   
   overriding
   function Terminated (Process: Managed_Process) 
                       return Boolean;

   overriding
   procedure Wait_Terminated (Process  : in     Managed_Process;
                              Timeout  : in     Duration;
                              Timed_Out:    out Boolean;
                              Status   :    out Exit_Status);
   
   overriding
   procedure Kill (Process: in out Managed_Process);
   
   overriding
   procedure Nuke (Process: in out Managed_Process);
   
   overriding
   procedure Finalize (Process: in out Managed_Process);
   
end Child_Processes.Managed;
