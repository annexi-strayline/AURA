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

with Ada.Streams;
with Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

with Unit_Names;
with Workers;
with Workers.Reporting;
with Registrar.Queries;
with Registrar.Library_Units;
with Registrar.Registration;

package body Configuration is
   
   package WWU    renames Ada.Strings.Wide_Wide_Unbounded;
   package UBS    renames Ada.Strings.Unbounded;
   package Reg_Qs renames Registrar.Queries;
   
   New_Line: Character renames Workers.Reporting.New_Line;
   
   subtype Unit_Name    is Unit_Names.Unit_Name;
   subtype Subsystem    is Registrar.Subsystems.Subsystem;
   subtype Library_Unit is Registrar.Library_Units.Library_Unit;
   
   procedure Assert (Check: in Boolean; Message: in String)
     renames Ada.Assertions.Assert;
   
   --
   -- Utilities
   --
   
   procedure Generate_Basic_Config (Target: in out Subsystem) is separate;
   -- Shall be called only when both the configuration unit and the manifest
   -- do not exist. This creates a file for the configuration unit, and writes
   -- an empty package specification to that file, and then enters the created
   -- unit with the registrar.
   
   procedure Complete (Target: in out Subsystem);
   -- Sets State to Available and updates the subsystem in the Registry
   
   --
   -- Steps (see Configure's spec on Configuration_Pass)
   --
   
   -- Each step calls the next appropriate step (or submits a deferred order
   -- for the next step) on success
   
   procedure Step_1  (Target: in out Subsystem);
   procedure Step_2  (Target: in out Subsystem);
   procedure Step_3a (Target: in out Subsystem);
   procedure Step_3b (Target: in out Subsystem);
   procedure Step_4  (Target: in out Subsystem);
   
   
   ------------------
   -- Config_Order --
   ------------------
   
   type Config_Order is new Workers.Work_Order with
      record
         Target: Subsystem;
      end record;
   
   overriding procedure Execute (Order: in out Config_Order);
   overriding function  Image   (Order: Config_Order) return String;

   function Image (Order: Config_Order) return String
     is (    "[Config_Order]" & New_Line
           & " Subsystem: " & Order.Target.Name.To_UTF8_String);
   
   procedure Execute (Order: in out Config_Order) is
   begin
      Step_1 (Order.Target);
   end Execute;
   
   ----------------------------
   -- Step_3a Deferral Order --
   ----------------------------
   
   -- Step 2 involves generating a new configuration unit, which then must be
   -- entered with the Registrar. This deferral order for the next step (3a)
   -- is used to wait on that registration process.
   --
   -- Steps skipping step 2 can simply go to Step 3a directly.
   
   type Step_3a_Deferral is new Workers.Work_Order with
      record
         Target: Subsystem;
      end record;
   
   overriding function  Image   (Order: Step_3a_Deferral) return String;
   overriding procedure Execute (Order: in out Step_3a_Deferral);
   
   function Image (Order: Step_3a_Deferral) return String
     is (    "[Config_Order - Step 3a Deferral]" & New_Line
           & " Subsystem: " & Order.Target.Name.To_UTF8_String);
   
   procedure Execute (Order: in out Step_3a_Deferral) is
   begin
      Step_3a (Order.Target);
   end Execute;
   
   
   --
   -- Utilities and Interface Implementations
   --
   
   ----------------------
   -- Config_Unit_Name --
   ----------------------
   
   function Config_Unit_Name (Target: Registrar.Subsystems.Subsystem)
                             return Unit_Names.Unit_Name
   is
      use Unit_Names;
   begin
      if Target.Name.To_String = "aura" then
         return Set_Name ("AURA.Root");
      else
         return "AURA." & Target.Name;
      end if;
   end Config_Unit_Name;
   
   ------------------------
   -- Manifest_Unit_Name --
   ------------------------
   
   function Manifest_Unit_Name (Target: Registrar.Subsystems.Subsystem)
                               return Unit_Name
   is
      use Unit_Names;
   begin
      pragma Assert (Target.Name.To_String /= "aura");
      -- The root configuration package can't actually have a manifest. So
      -- this should never happen. Any subprogram looking for a manifest should
      -- know about this special case and should not be calling this function

      return Target.Name & ".AURA";
   end Manifest_Unit_Name;
   
   --------------
   -- Complete --
   --------------
   
   procedure Complete (Target: in out Subsystem) is
      use Registrar.Subsystems;
   begin
      Target.State := Available;
      Registrar.Registration.Update_Subsystem (Target);
   end Complete;
   
   
   ------------------------
   -- Configuration_Pass --
   ------------------------
   
   procedure Configuration_Pass is
      use Registrar.Subsystems;
      
      Aquired: constant Subsystem_Sets.Set
        := Registrar.Queries.Aquired_Subsystems;
      
      Order: Config_Order;
   begin
      Configuration_Progress.Increase_Total_Items_By 
        (Natural (Aquired.Length));
      
      Order.Tracker := Configuration_Progress'Access;
      
      for Subsys of Aquired loop
         Order.Target := Subsys;
         Workers.Enqueue_Order (Order);
      end loop;
      
   end Configuration_Pass;
   
   --------------------------------
   -- Process_Root_Configuration --
   --------------------------------
   
   procedure Process_Root_Configuration is
      Order: Config_Order;
   begin
      Configuration_Progress.Increase_Total_Items_By (1);
      Order.Tracker := Configuration_Progress'Access;
      
      -- Realistically it should not be possible to get here (unless sequencing
      -- was wrong) without the AURA subsystem being registered. We won't check
      -- for that explicitly, since if it happens, it has to be a programming
      -- error that the user cannot directly induce.
      --
      -- When compiled with assertions enabled, the precondition on
      -- Lookup_Subsystem will check for that
      
      Order.Target 
        := Registrar.Queries.Lookup_Subsystem 
          (Unit_Names.Set_Name("aura"));
      
      -- The status will be Available, but the later processes will make sure
      -- this is not changed for this special case
      
      Workers.Enqueue_Order (Order);
   end Process_Root_Configuration;
   
   --
   -- Step implementation Stubs
   --
   
   procedure Step_1  (Target: in out Subsystem) is separate;
   procedure Step_2  (Target: in out Subsystem) is separate;
   procedure Step_3a (Target: in out Subsystem) is separate;
   procedure Step_3b (Target: in out Subsystem) is separate;
   procedure Step_4  (Target: in out Subsystem) is separate;
   
   
end Configuration;
