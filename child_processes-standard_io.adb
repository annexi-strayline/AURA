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

with Ada.Calendar;

package body Child_Processes.Standard_IO is
   
   ----------------
   -- Set_Handle --
   ----------------
   
   procedure Set_Handle (Stream: in out Standard_IO_Stream;
                         Handle: in     Platform.Stream_Handle)
   is
   begin
      Stream.Handle := Handle;
   end Set_Handle;
   
   ------------
   -- Handle --
   ------------
   
   function Handle (Stream: in out Standard_IO_Stream) 
                   return Platform.Stream_Handle 
     is (Stream.Handle);
   
   -----------------
   -- Set_Timeout --
   -----------------
   
   procedure Set_Timeout (Stream : in out Standard_IO_Stream;
                          Timeout: in     Duration)
   is
   begin
      Stream.Timeout := Timeout;
   end Set_Timeout;
   
   ----------
   -- Read --
   ----------
   
   procedure Read (Stream: in out Standard_IO_Stream;
                   Item  :    out Stream_Element_Array;
                   Last  :    out Stream_Element_Offset)
   is
      use Ada.Calendar;
      
      First_Attempt: Time;
      Timed_Out: Boolean;
   begin
      
      -- First try the whole thing in one shot
      Platform.Read_Buffer
        (Stream => Stream.Handle,
         Buffer => Item,
         Last   => Last);
      
      if Last = Item'Last then
         -- That was easy!
         return;
         
      else
         -- Otherwise we need to go into a wait loop
         Last := Item'First - 1;
         First_Attempt := Clock;
      end if;
      
      while Last < Item'Last 
        and then (if Stream.Timeout > 0.0 then
                     Clock <= (First_Attempt + Stream.Timeout))
      loop
         Platform.Wait_Can_Read (Stream    => Stream.Handle,
                                 Timeout   => Stream.Timeout,
                                 Timed_Out => Timed_Out);
         
         if Timed_Out then
            -- No question that this is timeout
            return;
            
         else
            -- Try to read in the next chunk
            Platform.Read_Buffer
              (Stream => Stream.Handle,
               Buffer => Item(Last + 1 .. Item'Last),
               Last   => Last);
            
         end if;
      end loop;      
      
   end Read;
   
   
   -----------
   -- Write --
   -----------
   
   overriding
   procedure Write (Stream: in out Standard_IO_Stream;
                    Item  : in     Stream_Element_Array)
   is
      use Ada.Calendar;
      
      First_Attempt: Time := Clock;
      Last: Stream_Element_Offset := Item'First - 1;
      Timed_Out: Boolean;
      
      procedure Timeout_Raise is
      begin
         raise Storage_Error with "Platform stream buffer full";
      end Timeout_Raise;

   begin
      
      loop
         -- Much simpler without a buffer!
         Platform.Write_Buffer (Stream => Stream.Handle,
                                Buffer => Item(Last + 1 .. Item'Last),
                                Last   => Last);
         
         exit when Last = Item'Last;
         
         if Stream.Timeout > 0.0 
           and then Clock > (First_Attempt + Stream.Timeout)
         then
            Timeout_Raise;
         end if;
         
         Platform.Wait_Can_Write (Stream    => Stream.Handle,
                                  Timeout   => Stream.Timeout,
                                  Timed_Out => Timed_Out);
         
         if Timed_Out then
            Timeout_Raise;
         end if;
         
      end loop;
      
   end Write;
   
   --------------------
   -- Read_Immediate --
   --------------------
   
   procedure Read_Immediate (Stream: in out Standard_IO_Stream;
                             Item  :    out Stream_Element_Array;
                             Last  :    out Stream_Element_Offset)
   is begin
      Platform.Read_Buffer (Stream => Stream.Handle,
                            Buffer => Item,
                            Last   => Last);
   end Read_Immediate;
   
end Child_Processes.Standard_IO;
