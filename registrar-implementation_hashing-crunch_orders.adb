------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2021, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Registrar.Source_Files;

separate (Registrar.Implementation_Hashing)

package body Crunch_Orders is
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Crunch_Order) return String is
     ("[Crunch_Order] (Registrar.Implementation_Hashing)" & New_Line
        & " Target Unit: "
        & Order.Source_Set.all
          (Order.Collection_Set.all(Order.Collector).Target_Unit)
        .Name.To_UTF8_String);
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Crunch_Order) is
      
      procedure Free_Hash_Queue is new Ada.Unchecked_Deallocation
        (Object => Hash_Queues.Queue,
         Name   => Hash_Queue_Access);
      
      Collector: Collection_Unit;
      
      Spec_Hash    : Stream_Hashing.Hash_Type;
      Crunched_Hash: Stream_Hashing.Hash_Type;
      
      
      procedure Update_Hash (Item: in out Library_Units.Library_Unit)
      is begin
         Item.Specification_Hash  := Spec_Hash;
         Item.Implementation_Hash := Crunched_Hash;
      end Update_Hash;
      
   begin
      -- Extract the Collector from the Collection_Set
      Collector := Order.Collection_Set.all(Order.Collector);
      
      Stream_Hashing.Collective.Compute_Collective_Hash
        (Hash_Queue      => Collector.Hash_Queue.all, 
         Collective_Hash => Crunched_Hash);
      
      declare
         use Registrar.Source_Files;
         
         Spec_File: Source_File_Access
           renames Order.Source_Set.all(Collector.Target_Unit).Spec_File;
      begin
         if Spec_File /= null then
            Spec_Hash := Spec_File.Hash;
         end if;
         
         -- Otherwise the hash should be assumed to be invalid since this unit
         -- does not have a specification
      end;
      
      -- Modifiy in-place
      Registry.All_Library_Units.Modify 
        (Match   => Order.Source_Set.all(Collector.Target_Unit),
         Process => Update_Hash'Access);
      
      Free_Hash_Queue (Collector.Hash_Queue);
   end Execute;
   
   -------------------
   -- Phase_Trigger --
   -------------------
   
   procedure Phase_Trigger (Order: in out Crunch_Order) is
      
      New_Order: Cleanup_Order;
   begin
      -- We can leave the Beacon now, since we are not relying on any more
      -- phase triggers after this one. The Cleanup_Order simply deallocates
      -- the sets we were using up to this point
      Guard_Beacon.Leave;
      
      New_Order.Source_Set     := Order.Source_Set;
      New_Order.Collection_Set := Order.Collection_Set;
      Workers.Enqueue_Order (New_Order);
   end Phase_Trigger;
   
end Crunch_Orders;
