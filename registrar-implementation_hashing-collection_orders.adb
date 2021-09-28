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

separate (Registrar.Implementation_Hashing)

package body Collection_Orders is
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Collection_Order) return String is
     ("[Collection_Order] (Registrar.Implementation_Hashing)" & New_Line
        & " Source Unit: " 
        & Order.Source_Set.all(Order.Source).Name.To_UTF8_String 
        & New_Line 
        & " Target Unit: " 
        & Order.Source_Set.all
          (Order.Collection_Set.all(Order.Collector).Target_Unit)
        .Name.To_UTF8_String);
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Collection_Order) is
      use Library_Units;
      use type Registrar.Source_Files.Source_File_Access;
      
      Source_Unit: Library_Unit renames Order.Source_Set.all(Order.Source);
      
      HQ: constant Hash_Queue_Access 
        := Order.Collection_Set.all(Order.Collector).Hash_Queue;
      
   begin
      -- Submit all available body hashes to the designated queue
      if Source_Unit.Body_File /= null then
         pragma Assert (Source_Unit.Kind /= Subunit);
         HQ.Enqueue (Source_Unit.Body_File.Hash);
      end if;
      
      -- Note that if there is no body file, such as for spec-only units, there
      -- will be no hashes in the queue. However, the generated collective hash
      -- will be the "default" hash. This is fine, since body-less units are
      -- are evaluated according to their spec hash only.
      
      for SF of Source_Unit.Subunit_Bodies loop
         HQ.Enqueue (SF.Hash);
      end loop;
      
   end Execute;
   
   -------------------
   -- Phase_Trigger --
   -------------------
   
   procedure Phase_Trigger (Order: in out Collection_Order) is
      Next_Order: Crunch_Orders.Crunch_Order;
   begin
      
      Next_Order.Tracker := Crunch_Phase_Progress'Access;
      -- Note that the tracker is set in the Hash_Subsystem main subprogram
      
      Next_Order.Source_Set     := Order.Source_Set;
      Next_Order.Collection_Set := Order.Collection_Set;
      
      for Unit_Cursor in Order.Collection_Set.Iterate loop
         Next_Order.Collector := Unit_Cursor;
         Workers.Enqueue_Order (Next_Order);
      end loop;
   end Phase_Trigger;
   
end Collection_Orders;
