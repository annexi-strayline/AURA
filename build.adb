------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
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

with Ada.Streams.Stream_IO;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Repositories;
with Registrar.Last_Run;
with Registrar.Queries;
with Registrar.Subsystems;
with Registrar.Source_Files;
with Registrar.Registration;
with Workers, Workers.Reporting;
with Unit_Names, Unit_Names.Sets;


package body Build is
   
   use type Registrar.Library_Units.Library_Unit_Kind;
   use type Registrar.Source_Files.Source_File_Access;
   
   New_Line: Character renames Workers.Reporting.New_Line;
   
   --
   -- Utilities
   --
   
   ----------------------
   -- Object_File_Name --
   ----------------------
   
   function Object_File_Name (Unit: Registrar.Library_Units.Library_Unit)
                             return String
   is
      use Ada.Directories;
      use Repositories;
      use Registrar.Subsystems;
      
      Unit_Simple_Name: constant String
        := (if Unit.Body_File /= null then
               Simple_Name (Unit.Body_File.Full_Name)
            else
               Simple_Name (Unit.Spec_File.Full_Name));
      
      Subsys: constant Subsystem
        := Registrar.Queries.Lookup_Subsystem (Unit.Name.Subsystem_Name);
      

   begin
      if Subsys.AURA 
        and then Extract_Repository (Subsys.Source_Repository).Format = System
      then
         declare
            Subsys_Name: constant String := Subsys.Name.To_UTF8_String;
         begin
            return Current_Directory & '/' & Subsys_Name & '/'
              & Subsys_Name & ".so";
         end;
         
      else
         return Build_Root & '/' & Base_Name (Unit_Simple_Name) & ".o";
      end if;
   end Object_File_Name;
   
   -------------------
   -- ALI_File_Name --
   -------------------
   
   function ALI_File_Name (Unit: Registrar.Library_Units.Library_Unit)
                          return String
   is
      use Ada.Directories;
   begin
      return Build_Root 
        & '/' & Base_Name (Simple_Name (Object_File_Name (Unit))) & ".ali";
   end ALI_File_Name;
   
   
   --
   -- Build_Configuration
   --
   
   Last_Config_Store: constant String 
     := Ada.Directories.Current_Directory & "/.aura/last_build.dat";
   
   ----------------------------
   -- Load_Last_Build_Config --
   ----------------------------
   
   procedure Load_Last_Build_Config (Configuration: out Build_Configuration)
   is
      use Ada.Streams.Stream_IO;
      
      Config_File: File_Type;
   begin
      Open (File => Config_File,
            Mode => In_File,
            Name => Last_Config_Store);
      
      Build_Configuration'Read (Stream (Config_File), Configuration);
      
      Close (Config_File);
   exception
      when others => null;
   end Load_Last_Build_Config;
   
   ------------------------
   -- Store_Build_Config --
   ------------------------
   
   procedure Store_Build_Config (Current_Config: Build_Configuration) is
      use Ada.Streams.Stream_IO;
      
      Config_File: File_Type;
   begin
      if not Ada.Directories.Exists (".aura") then
         Ada.Directories.Create_Directory (".aura");
      end if;
      
      if not Ada.Directories.Exists (Last_Config_Store) then
         Create (File => Config_File,
                 Mode => Out_File,
                 Name => Last_Config_Store);
      else
         Open (File => Config_File,
               Mode => Out_File,
               Name => Last_Config_Store);
      end if;
      
      Build_Configuration'Write (Stream (Config_File), Current_Config);
      
      Close (Config_File);
      
   end Store_Build_Config;
   
   --
   -- Preparation
   --
   
   ----------------
   -- Init_Paths --
   ----------------
   
   procedure Init_Paths is
      use Ada.Directories;
      
   begin
      if Registrar.Last_Run.All_Library_Units.Is_Empty
        and then Exists (Build_Root)
      then
         Delete_Tree (Build_Root);
         Create_Path (Build_Output_Root);
         Create_Path (Build_Root);
         
      elsif not Exists (Build_Root) 
        or else not Exists (Build_Output_Root)
      then
         Create_Path (Build_Output_Root);
      end if;
      
   end Init_Paths;
   
   
   -------------------------------
   -- Hash_Compilation_Products --
   -------------------------------
   
   package Hash_Compilation_Orders is
      
      type Hash_Compilation_Order is new Workers.Work_Order with
         record
            Target: Registrar.Library_Units.Library_Unit;
         end record;
      
      -- The Hash_Compilation_Order does not generate any further orders.
      -- This ensures Direct_Hash_Compilatoion to execute an order directly
      -- without needing to deal with trackers
      
      overriding function  Image (Order: Hash_Compilation_Order) return String;
      overriding procedure Execute (Order: in out Hash_Compilation_Order);
      
      
   end Hash_Compilation_Orders;
   
   package body Hash_Compilation_Orders is separate;
   
   --------------------------------------------------
   procedure Hash_Compilation_Products is
      use Registrar.Library_Units;
      
      Order: Hash_Compilation_Orders.Hash_Compilation_Order;
      Selected_Units: Registrar.Library_Units.Library_Unit_Sets.Set;
      All_Units: constant Registrar.Library_Units.Library_Unit_Sets.Set
        := Registrar.Queries.Entered_Library_Units;
      
   begin
      Order.Tracker := Compilation_Hash_Progress'Access;
      
      -- Find our selected units for hashing. These are everything except
      -- Subunit kinds
      
      for Unit of All_Units loop
         pragma Assert (Unit.Kind /= Unknown);
         
         -- Compiled units also need to be rehashed
         
         if Unit.State in Available | Compiled 
           and then Unit.Kind /= Subunit 
         then
            Selected_Units.Include (Unit);
         end if;
      end loop;
      
      Order.Tracker.Increase_Total_Items_By (Natural (Selected_Units.Length));
      
      -- Dispatch
      for Unit of Selected_Units loop
         Order.Target := Unit;
         Workers.Enqueue_Order (Order);
      end loop;
   end Hash_Compilation_Products;
   
   --------------------------------------
   -- Direct_Hash_Compilation_Products --
   --------------------------------------
   
   procedure Direct_Hash_Compilation_Products
     (Unit: in Registrar.Library_Units.Library_Unit)
   is
      Direct_Order: Hash_Compilation_Orders.Hash_Compilation_Order
        := (Tracker => null,
            Target  => Unit);
   begin
      Direct_Order.Execute;
   end Direct_Hash_Compilation_Products;
   
   
   ----------------------------
   -- Compute_Recompilations --
   ----------------------------
   
   package Recompilation_Check_Orders is
      
      protected type Recompilation_Set is
         
         procedure Enter_Subset 
           (Entry_Subset: in out Unit_Names.Sets.Set);
         -- Enters any items that are in Entry_Subset but not in the
         -- Recompilation_Set. Items that are already exist in the
         -- Recompilation_Set are deleted from the Entry_Subset.
         --
         -- The principal of operation is that a Recompilation_Check_Order
         -- gets the reverse dependency set for it's target unit, and then
         -- submits that set to Enter_Subset. On return, Entry_Subset
         -- will contain only the units that the order should then
         -- generate additional orders for. 
         --
         -- The approach eliminates cyclic recursion.
         
         function  Retrieve return Unit_Names.Sets.Set;
         -- Returns the entire Recompilation_Set
         
      private
         Master: Unit_Names.Sets.Set;
         Ret_Guard: Boolean := False;
      end Recompilation_Set;
      
      type Recompilation_Set_Access is access Recompilation_Set;
      
      -------------------------------
      -- Recompilation_Check_Order --
      -------------------------------
      
      -- Recompilation_Check_Orders take a Target unit name, and recursively
      -- submits additional orders for each dependent unit if it is determined
      -- that the Target library unit requires recompilation.
      --
      -- How the order determines if the Target unit requires recompilation
      -- depends on the Mode component.
      --
      -- At the end of the process, the Phase_Trigger takes the generated
      -- set of unit names and marks all corresponding units in
      -- All_Library_Units as "Available" so that they will be recompiled
      -- in the compilation phase
      
      type Processing_Mode is 
        (Test,
         -- The designated library unit needs to be checked. This means
         -- checking the compilation hash, specification hash, and
         -- implementation hash against the Last_Run set.
         --
         -- This mode is only set from the original dispatch
         -- (Build.Compute_Recompilations)
         
         Set);
         -- The designated unit name must be entered into the Recompilation
         -- Set, along with reverse dependencies, recursively.
         --
         -- This mode is always set for any recursively dispatched order.
      
      type Recompilation_Check_Order is new Workers.Work_Order with
         record
            Target    : Unit_Names.Unit_Name;
            Mode      : Processing_Mode;
            Recomp_Set: Recompilation_Set_Access;
         end record;
      
      overriding function  Image (Order: Recompilation_Check_Order)
                                 return String;
      
      overriding procedure Execute (Order: in out Recompilation_Check_Order);
      
      overriding procedure Phase_Trigger 
        (Order: in out Recompilation_Check_Order);
      
   end Recompilation_Check_Orders;
   
   package body Recompilation_Check_Orders is separate;
   
   
   --------------------------------------------------
   procedure Compute_Recompilations (Configuration: Build_Configuration) is
      use Registrar.Library_Units;
      use Recompilation_Check_Orders;
      
      package LU_Keyed_Ops renames 
        Registrar.Library_Units.Library_Unit_Sets_Keyed_Operations;
      
      procedure Set_Available (Unit: in out Library_Unit) is
      begin
         Unit.State := Available;
      end Set_Available;
      
      Last_Config: Build_Configuration;
      
      All_Units: Library_Unit_Sets.Set
        := Registrar.Queries.Entered_Library_Units;
      
      Target_Set: Unit_Names.Sets.Set;
      
      Order: Recompilation_Check_Order;
      Beacon_OK: Boolean;
   begin
      Compute_Recompilations_Completion.Approach (Beacon_OK);
      if not Beacon_OK then return; end if;
      
      Load_Last_Build_Config (Configuration => Last_Config);
      
      if Registrar.Last_Run.All_Library_Units.Is_Empty 
        or else Last_Config /= Configuration
      then
         -- All units must be recompiled. The actual Compile process will
         -- ensure any residual objects are deleted
         
         for C in All_Units.Iterate loop
            declare
               use Repositories;
               use Registrar.Subsystems;
               
               USS: constant Subsystem
                 := Registrar.Queries.Lookup_Subsystem 
                   (All_Units(C).Name.Subsystem_Name);
            begin
               if not (USS.AURA and then 
                         Extract_Repository(USS.Source_Repository).Format 
                         = System)
               then
                  LU_Keyed_Ops.Update_Element_Preserving_Key
                    (Container => All_Units,
                     Position  => C,
                     Process   => Set_Available'Access);
               end if;
            end;
         end loop;
         
         Registrar.Registration.Update_Library_Unit_Subset (All_Units);
         Compute_Recompilations_Completion.Leave;
         return;
      end if;
      
      
      -- Only re-evaluate Compiled items, since the purpose here is to select
      -- which "Compiled" units need to be pushed back to "Available" so that
      -- they will be compiled.
      
      for Unit of All_Units loop
         if Unit.Kind in Package_Unit | Subprogram_Unit | External_Unit
           and then Unit.State = Compiled 
         then
            Target_Set.Insert (Unit.Name);
         end if;
      end loop;
      
      Order.Tracker    := Compute_Recompilations_Progress'Access;
      Order.Mode       := Test;
      Order.Recomp_Set := new Recompilation_Set;
      
      Order.Tracker.Increase_Total_Items_By (Natural (Target_Set.Length));
      
      for Name of Target_Set loop
         Order.Target := Name;
         Workers.Enqueue_Order (Order);
      end loop;
      
   end Compute_Recompilations;
   
end Build;
