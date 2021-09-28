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

-- This package contains all of the platform-specific abstractions

private with Interfaces.C;

private package Child_Processes.Platform is
   
   type Process_ID    is private;
   type Stream_Handle is private;
   
   procedure Create_Process (Image_Path       : in     String;
                             Arguments        : in     String;
                             Working_Directory: in     String;
                             
                             ID               :    out Process_ID;
                             Standard_Input   :    out Stream_Handle;
                             Standard_Output  :    out Stream_Handle;
                             Standard_Error   :    out Stream_Handle);
   -- Creates the process and sets-up the IO streams. Before executing
   -- the image, the newly created process' working directory is set to
   -- Working_Directory.
   --
   -- Arguments should be of the standard command-line format, space-separated.
   -- The binding will re-arrange these into the appriate repsentation as
   -- required by the platform
   --
   -- Launch_Failure is raised in the if forking of a new process fails.
   
   procedure Wait_Termination (ID       : in     Process_ID;
                               Exit_Code:    out Exit_Status);
   -- Waits indefinately for the created process to terminate. Exit_Code
   -- is either set to Success or Failure
   
   procedure Kill (ID: in Process_ID);
   -- Sends SIGTERM to the process
   
   procedure Nuke (ID: in Process_ID);
   -- Sends SIGKILL to the process;
   
   procedure Read_Buffer (Stream: in     Stream_Handle;
                          Buffer:    out Stream_Element_Array;
                          Last  :    out Stream_Element_Offset);
   -- This operation never blocks
   
   procedure Write_Buffer (Stream: in     Stream_Handle;
                           Buffer: in     Stream_Element_Array;
                           Last  :    out Stream_Element_Offset);
   -- This operation never blocks
   
   procedure Wait_Can_Read  (Stream   : in     Stream_Handle; 
                             Timeout  : in     Duration;
                             Timed_Out:    out Boolean);
   
   procedure Wait_Can_Write (Stream   : in   Stream_Handle; 
                             Timeout  : in   Duration;
                             Timed_Out:  out Boolean);
   -- Blocks until the stream is able to read resp. write
   -- If Timeout is 0, the timeout is indefinate.
   --
   -- Timed_Out is set to False iff a timeout occurs before the
   -- stream is ready.
   
   procedure Close_Stream (Handle: in Stream_Handle);
   
private
   
   -- CRTL Types (x86_64 FreeBSD) --
   -- These are all defined here rather than "as needed" (with the rest being
   -- in the body) to make the body as platform-agnostic as possible. Only the
   -- private part of this spec should be different between most unicies
   
   use type Interfaces.Integer_64;
   use type Interfaces.C.int;
   use type Interfaces.C.long;
   
   type id_t is new Interfaces.Integer_64;
   type pid_t is new id_t;
   
   type time_t is new Interfaces.Integer_64;
   type suseconds_t is new Interfaces.C.long;
   
   type timeval is
      record
         tv_sec : time_t;
         tv_usec: suseconds_t;
      end record;
   
   type ssize_t is new Interfaces.Integer_64;
   
   -- Ada types --
   
   type Process_ID    is new pid_t;
   type Stream_Handle is new Interfaces.C.int;
   -- (via manpages)
   -- standard file descriptor
   
end Child_Processes.Platform;
