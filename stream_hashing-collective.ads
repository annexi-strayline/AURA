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

-- This package provides infrastructure for computing a single hash based on
-- a collection of individual stream hashes, and doing so in a efficient
-- distributed manor

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Ordered_Sets;

package Stream_Hashing.Collective is
   
   package Hash_Collections is new Ada.Containers.Ordered_Sets (Hash_Type);
   -- By making the hash collection an ordered set, the order of individual
   -- hashes is not significant
   
   -- Set based
   
   procedure Compute_Collective_Hash 
     (Collection     : in     Hash_Collections.Set;
      Collective_Hash:    out Hash_Type);
   
   -- Queue based
   
   package Hash_Queue_Interfaces is 
     new Ada.Containers.Synchronized_Queue_Interfaces (Hash_Type);
   
   package Hash_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Hash_Queue_Interfaces);
   
   procedure Compute_Collective_Hash 
     (Hash_Queue     : in out Hash_Queues.Queue;
      Collective_Hash:    out Hash_Type);
   
   -- Drains Hash_Queue into a collection, and then computes the hash.
   --
   -- The Queue is expected to not receive further enqueues during the call,
   -- and will stop draining as soon as there are no more items

   
   -- Rationale for providing a Synchronized Queue
   -- --------------------------------------------
   -- One possible approach to enabling distributed hashing could be to make
   -- a protected "collector" type, which would have an Add procedure.
   --
   -- In this case, it is more efficient to use a regular container type,
   -- in conjunction with a synchronized queue, that is because, especially
   -- as the collection grows, it will be faster to add a hash to the queue
   -- than it would be to enter it into the collection. By using a synchronized
   -- queue, the worker tasks will spend a minimum amount of time completing
   -- and submitting a single hash, with some master task then sequentially
   -- adding all the hashes before computing the collective hash.
   --
   -- This approach has some other secondary benefits, such as allowing the
   -- use of select statements to monitor both the queue, as well as a
   -- progress tracker
   
end Stream_Hashing.Collective;
