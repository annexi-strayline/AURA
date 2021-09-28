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

with Ada.Text_IO;

with Ada.Calendar; use Ada.Calendar;

with Child_Processes.Standard_IO;

procedure Child_Processes.Wait_And_Buffer 
  (Process  : in out Child_Process'Class;
   Poll_Rate: in     Duration;
   Timeout  : in     Duration;
   Output   :    out Buffer_Type;
   Error    :    out Buffer_Type;
   Timed_Out:    out Boolean;
   Status   :    out Exit_Status)
is
   use Standard_IO;
   type IO_Stream_Access is access all Standard_IO_Stream'Class with
     Storage_Size => 0;
   
   STDOUT: constant IO_Stream_Access 
     := IO_Stream_Access (Process.IO_Stream (Standard_Output));
   STDERR: constant IO_Stream_Access 
     := IO_Stream_Access (Process.IO_Stream (Standard_Error));
   
   
   package Stream_Buffers is
      
      type Stream_Buffer is new Root_Stream_Type with
         record
            Buffer: Stream_Element_Array (1 .. 512);
            -- Note there is a GNAT bug here where if this array is any
            -- larger than 512, To_String can't handle it.
            
            Level : Stream_Element_Offset := 0;
         end record;
      -- To reduce the number of system calls
      
      overriding 
      procedure Write (Stream: in out Stream_Buffer;
                       Item  : in     Stream_Element_Array)
        is null;
      
      overriding
      procedure Read (Stream: in out Stream_Buffer;
                      Item  :    out Stream_Element_Array;
                      Last  :    out Stream_Element_Offset);
      
      not overriding
      function To_String (Stream: aliased in out Stream_Buffer)
                         return String;
   end Stream_Buffers;
   
   package body Stream_Buffers is
      
      ----------
      -- Read --
      ----------
      
      -- The purpose of Read is to use the Ada stream attributes for String
      -- to voncert from out Stream_Element_Array to a String. Obviously this
      -- call only happens here, so we are strict in what we expect for the
      -- parameters -> that they exactly fit the size of the buffer
      
      procedure Read (Stream: in out Stream_Buffer;
                      Item  :    out Stream_Element_Array;
                      Last  :    out Stream_Element_Offset)
      is 
         Buffer_Slice: Stream_Element_Array 
           renames Stream.Buffer (Stream.Buffer'First .. Stream.Level);
      begin
         Item := Stream.Buffer (Stream.Buffer'First .. Stream.Level);
         Last := Item'Last;
         Stream.Level := 0;
      end Read;
      
      ---------------
      -- To_String --
      ---------------
      
      function To_String (Stream: aliased in out Stream_Buffer)
                         return String
      is
         pragma Assert (Character'Stream_Size = Stream_Element'Size);
         -- To be honest, we're pretty darn sure this is always going to be
         -- one Character per Stream_Element, but this is good form.
      begin
         return S: String (1 .. Natural (Stream.Level)) do
            String'Read (Stream'Access, S);
         end return;
      end To_String;
   end Stream_Buffers;
   
   use Stream_Buffers;
   
   
   Out_Buffer: aliased Stream_Buffer;
   Err_Buffer: aliased Stream_Buffer;
   
   Start: Time;
   
   Discard: Boolean;
   
   
   procedure Drain_Streams is
   begin
      loop
         STDOUT.Read_Immediate (Item => Out_Buffer.Buffer,
                                Last => Out_Buffer.Level);
         
         STDERR.Read_Immediate (Item => Err_Buffer.Buffer,
                                Last => Err_Buffer.Level);
         
         exit when Out_Buffer.Level = 0 and Err_Buffer.Level = 0;
         
         if Out_Buffer.Level > 0 then
            Append (Buffer => Output,
                    Item   => Out_Buffer.To_String);
         end if;
         
         if Err_Buffer.Level > 0 then
            Append (Buffer => Error,
                    Item   => Err_Buffer.To_String);
         end if;
      end loop;
   end Drain_Streams;
   
begin
   Timed_Out := False;
   Start     := Clock;
   
   Output := Empty_Buffer;
   Error  := Empty_Buffer;
   
   loop
      
      Drain_Streams;
      
      if Process.Terminated then
         
         -- Get the status
         Process.Wait_Terminated (Timeout   => 0.0,
                                  Timed_Out => Discard,
                                  Status    => Status);
         
         -- Drain one last time
         Drain_Streams;
         return;
      end if;
      
      delay Poll_Rate;
         
      exit when Clock > Start + Timeout;
   end loop;
   
   Drain_Streams;
   Timed_Out := True;
   
end Child_Processes.Wait_And_Buffer;
