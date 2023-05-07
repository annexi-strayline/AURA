------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2023, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Ada.Directories;
with Ada.Assertions;

with Workers, Workers.Reporting;
with Unit_Names;
with User_Queries;
with Repositories;
with Registrar.Queries;
with Registrar.Registration;
with Registrar.Library_Units;

package body Checkout is
   
   procedure Assert (Check: in Boolean; Message: in String)
     renames Ada.Assertions.Assert;
   
   New_Line: Character renames Workers.Reporting.New_Line;
   
   -------------------------
   -- Parse_Checkout_Spec --
   -------------------------
   
   function Parse_Checkout_Spec (Unit: Registrar.Library_Units.Library_Unit)
                                return Repositories.Repository_Index
   is separate;
   
   -------------------------
   -- Write_Checkout_Spec --
   -------------------------
   
   procedure Write_Checkout_Spec (SS: in Registrar.Subsystems.Subsystem) is 
     separate;
   
   
   --
   -- Checkout_Orders
   --
   
   package Checkout_Orders is
      
      type Checkout_Order is new Workers.Work_Order with
         record
            Target: Registrar.Subsystems.Subsystem;
         end record;
      
      overriding procedure Execute (Order: in out Checkout_Order);
      overriding function  Image   (Order: Checkout_Order) return String;
      
   end Checkout_Orders;
   
   package body Checkout_Orders is separate;
   
   
   -------------------
   -- Checkout_Pass --
   -------------------
   
   procedure Checkout_Pass is
      use Unit_Names;
      use Registrar.Subsystems;
      use Registrar.Library_Units;
      
      package Queries renames Registrar.Queries;
      
      Order: Checkout_Orders.Checkout_Order;
      Candidates: Subsystem_Sets.Set := Queries.Requested_Subsystems;

   begin
      -- Get the tracker in order
      Order.Tracker := Checkout_Pass_Progress'Access;
      
      pragma Assert (Order.Tracker.Is_Complete);
      -- This tracker should always be waited on for completion before
      -- re-invoking checkout cycle
      
      Order.Tracker.Increase_Total_Items_By (Natural (Candidates.Length));
      
      for SS of Candidates loop
         Order.Target := SS;
         Workers.Enqueue_Order (Order);
      end loop;
      
   end Checkout_Pass;
   
   
end Checkout;
