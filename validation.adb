------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
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

with Workers;
with Repositories;
with Registrar.Queries;
with Registrar.Subsystems;

package body Validation is
   
   package VSD_Orders is
      
      type Validate_Subsystem_Dependencies_Order is
        new Workers.Work_Order with
         record
            Target: Registrar.Subsystems.Subsystem;
         end record;
      
      overriding function  Image (Order: Validate_Subsystem_Dependencies_Order)
                                 return String;
      overriding procedure Execute 
        (Order: in out Validate_Subsystem_Dependencies_Order);
      
   end VSD_Orders;
   
   package body VSD_Orders is separate;
   
   -------------------------------------
   -- Validate_Subsystem_Dependencies --
   -------------------------------------
   
   procedure Validate_Subsystem_Dependencies is
      use VSD_Orders;
      use Repositories;
      use Registrar.Subsystems;
      
      Avail_Subsys: constant Subsystem_Sets.Set
        := Registrar.Queries.Available_Subsystems;
      Order: Validate_Subsystem_Dependencies_Order;
   begin
      -- Note that if we get to validation, there should be no subsystems that
      -- are not "Available"
      
      -- Find all the subsystems we need to check now so that we can set the
      -- tracker appropriately
      
      for SS of Avail_Subsys loop
         if Extract_Repository (SS.Source_Repository).Format = System then
            Check_Subset.Insert (SS);
         end if;
      end loop;
      
      if Check_Subset.Is_Empty then return; end if;
      
      Validate_Subsystem_Dependencies_Progress.Increase_Total_Items_By 
        (Natural (Check_Subset.Length));
      
      for SS of Check_Subset loop
         Order.Target := SS;
         Workers.Enqueue_Order (Order);
      end loop;
      
      -- Not much point in freeing Avail_Subsys if we get an exception, since
      -- getting an exception here will definately end the program shortly
      -- after
   end Validate_Subsystem_Dependencies;
   
end Validation;
