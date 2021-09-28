------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019-2020, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- This package presents a high-level, (mostly) platform independent access
-- to child processes - such as compilation, auto_configure execution, and
-- SCM back-ends

with Ada.Streams; use Ada.Streams;
with Ada.Finalization;

package Child_Processes is
   
   Spawn_Failure: exception;
   -- Raised if a child process cannot be initialy created (not if the child
   -- itself fails)
   
   
   type IO_Stream_Selector is
     (Standard_Input,
      Standard_Output,
      Standard_Error);
   -- In the perspective of the launched process
   
   type Exit_Status is (Success, Failure);
   
   type Child_Process is limited interface;
   
   ------------------
   -- Standard I/O --
   ------------------
   
   -- All standard output streams are non-blocking, and will raise an
   -- End_Error if the stream operation cannot be satisfied.
   --
   -- All standard output streams are also buffered. Failed reads are
   -- lossless.
   --
   -- Any reads to the output streams will also refil the buffer as much as
   -- possible.
   
   function IO_Stream (Process : in out Child_Process;
                       Selector: in     IO_Stream_Selector) 
                      return not null access Root_Stream_Type'Class
     is abstract;
   -- Returns a Stream access for the selected stream
   
   procedure Set_Stream_Timeout (Process : in out Child_Process;
                                 Selector: in     IO_Stream_Selector;
                                 Timeout : in     Duration)
     is abstract;
   -- Sets the timeout period for reads/writes on the selected IO stream.
   --
   -- If Read times-out, any elements that had been successfully read are
   -- returned through the underlying Read operation of the Stream Type.
   -- (No exception is raised).
   --
   -- If Write times-out (generally rare), this indicates that the platform's
   -- buffer is full, and thus Storage_Error is raised. Some of the data may
   -- have been written.
   --
   -- A Timeout of 0.0 indicates an indefinite timeout. This is the default
   -- value for all streams
   
   function Terminated (Process: Child_Process) 
                       return Boolean
     is abstract; 
   -- True if the process has terminated
   
   procedure Wait_Terminated (Process  : in     Child_Process;
                              Timeout  : in     Duration;
                              Timed_Out:    out Boolean;
                              Status   :    out Exit_Status)
     is abstract;
   -- Waits until Terminated is true or else the Timeout expires.
   -- If the timeout expires, Timed_Out is True.
   -- Status is only valid if Timed_Out is False.
   --
   -- If Timeout = 0.0, Wait_Terminated waits forever
   
   procedure Kill (Process: in out Child_Process)
     is abstract;
   -- Commands the process to terminate. (eg SIGTERM)
   
   procedure Nuke (Process: in out Child_Process)
     is abstract;
   -- Obliterates the process immediately. (eg SIGKILL)
   
   ----------------------
   -- Process Creation --
   ----------------------
   
   function Spawn_Process (Image_Path        : String;
                           Arguments         : String;
                           Working_Directory : String)
                          return Child_Process'Class;
   -- Starts a new process that executes the image at Image_Path, and receives
   -- the arguments specified by Arguments.
   --
   -- Standard_Out and Standard_Error each have a buffer of Buffer_Size.
   --
   -- Under rare circumstances, a new process could not be created at all, in
   -- which case Launch_Failure is raised. 
   --
   -- Normally the new process can be created, but may fail at launch. This
   -- will result in termination of the process. Error information may be
   -- avilabile on the Standard_Error stream.
   
end Child_Processes;
