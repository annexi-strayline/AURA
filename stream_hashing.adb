------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
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

package body Stream_Hashing is
   
   --
   -- Hash_Type
   --
   
   ---------
   -- "<" --
   ---------
   
   function "<" (Left, Right: Hash_Type) return Boolean
     is (Modular_Hashing.SHA1.SHA1_Hash (Left)."<"
           (Modular_Hashing.SHA1.SHA1_Hash (Right)));
   
   ---------------
   -- To_String --
   ---------------
   
   function To_String (H: Hash_Type) return String 
     is (H.Hexadecimal (Lower_Case => False));
   
   --------------------
   -- Digrest_Stream --
   --------------------
   
   function Digest_Stream
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class) 
     return Hash_Type
   is
      use Ada.Streams;
      
      Buffer: Stream_Element_Array (1 .. 1024);
      Last  : Stream_Element_Offset := Buffer'Last;
      
      Engine: Modular_Hashing.SHA1.SHA1_Engine;
   begin
      
      while Last = Buffer'Last loop
         Stream.Read (Item => Buffer, Last => Last);
         Engine.Write (Buffer (Buffer'First .. Last));
      end loop;
      
      return Hash_Type'(Modular_Hashing.SHA1.SHA1_Hash (Engine.Digest) 
                        with null record);
      
   end Digest_Stream;
   
end Stream_Hashing;
