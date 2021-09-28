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

package body Stream_Hashing.Collective is
   
   ----------------------------- 
   -- Compute_Collective_Hash --
   -----------------------------
   
   procedure Compute_Collective_Hash 
     (Collection     : in     Hash_Collections.Set;
      Collective_Hash:    out Hash_Type)
   is
      Engine: aliased Modular_Hashing.SHA1.SHA1_Engine;
   begin
      for H of Collection loop
         Hash_Type'Write (Engine'Access, H);
      end loop;
      
      Collective_Hash 
        := Hash_Type'(Modular_Hashing.SHA1.SHA1_Hash(Engine.Digest) 
                      with null record);
      
   end Compute_Collective_Hash;
   
   
   --------------------------------------------------
   procedure Compute_Collective_Hash 
     (Hash_Queue     : in out Hash_Queues.Queue;
      Collective_Hash:    out Hash_Type)
   is 
      Collection: Hash_Collections.Set;
      New_Hash: Hash_Type;
   begin
      loop
         select
            Hash_Queue.Dequeue (New_Hash);
         else
            exit;
         end select;
         
         
         
         Collection.Include (New_Hash);
      end loop;
      
      Compute_Collective_Hash (Collection, Collective_Hash);
   end Compute_Collective_Hash;
   
end Stream_Hashing.Collective;
