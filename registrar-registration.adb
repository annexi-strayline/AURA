------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
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

with Ada.Strings.Unbounded;
with Ada.Assertions;

with Workers;
with Registrar.Registry;
with Registrar.Executive.Unit_Entry;
with Registrar.Registration.Unchecked_Deregister_Unit;

package body Registrar.Registration is
   
   --
   -- All Enter_Units simply fill and submit Work_Orders to the worker pool
   --
   
   ----------------
   -- Enter_Unit --
   ----------------
   
   procedure Set_File_Name 
     (Order:    out Executive.Unit_Entry.Unit_Entry_Order;
      File : in     Ada.Directories.Directory_Entry_Type)
   is 
   begin
      Executive.Unit_Entry.UBS.Set_Unbounded_String
        (Target => Order.File_Full_Name,
         Source => Ada.Directories.Full_Name (File));
   end Set_File_Name;
   
   ----------------------------------------------------------------------------
   
   -- Non-AURA units
   procedure Enter_Unit (File: in Ada.Directories.Directory_Entry_Type) is
      Order: Executive.Unit_Entry.Unit_Entry_Order 
        := (Tracker   => Entry_Progress'Access,
            AURA      => False,
            others    => <>);
   begin
      Set_File_Name (Order => Order, File => File);
      Entry_Progress.Increment_Total_Items;
      Workers.Enqueue_Order (Order);
   end Enter_Unit;
   
   ----------------------------------------------------------------------------
   
   -- AURA units
   procedure Enter_Unit 
     (File          : in Ada.Directories.Directory_Entry_Type;
      AURA_Subsystem: in Registrar.Subsystems.Subsystem)
   is 
      Order: Executive.Unit_Entry.Unit_Entry_Order 
        := (Tracker        => Entry_Progress'Access,
            AURA           => True,
            AURA_Subsystem => AURA_Subsystem,
            others         => <>);
   begin
      Set_File_Name (Order => Order, File => File);
      Entry_Progress.Increment_Total_Items;
      Workers.Enqueue_Order (Order);
   end Enter_Unit;
   
   ---------------------
   -- Enter_Directory --
   ---------------------
   
   procedure Enter_Directory 
     (Directory_Path: in String;
      Order_Template: in Executive.Unit_Entry.Unit_Entry_Order)
   is
      use Ada.Directories;
      Search: Search_Type;
      File  : Directory_Entry_Type;
      
      Order: Executive.Unit_Entry.Unit_Entry_Order := Order_Template;
      
      procedure Enter_Extension (Extension: in String) is
      begin
         Start_Search (Search    => Search,
                       Directory => Directory_Path,
                       Pattern   => "*." & Extension,
                       Filter    => (Ordinary_File => True, others => False));
         
         while More_Entries (Search) loop
            Get_Next_Entry (Search => Search, Directory_Entry => File);
            Set_File_Name (Order, File);
            Entry_Progress.Increment_Total_Items;
            Workers.Enqueue_Order (Order);
         end loop;
         
         End_Search (Search);
      end Enter_Extension;
      
   begin
      Order.Tracker := Entry_Progress'Access;
      
      Enter_Extension ("ads");
      Enter_Extension ("adb");
      Enter_Extension ("c");
   end Enter_Directory;
   
   --------------------------------------------------
   procedure Enter_Directory 
     (Directory     : in Ada.Directories.Directory_Entry_Type;
      AURA_Subsystem: in Subsystems.Subsystem)
   is
      use Ada.Directories;
   begin
      Enter_Directory (Directory_Path => Full_Name (Directory),
                       Order_Template => (AURA           => True,
                                          AURA_Subsystem => AURA_Subsystem,
                                          others         => <>));
   end Enter_Directory;
   
   ----------------
   -- Enter_Root --
   ----------------
   
   procedure Enter_Root is
      use Ada.Directories;
   begin
      Enter_Directory (Directory_Path => Full_Name (Current_Directory),
                       Order_Template => (AURA           => False,
                                          others         => <>));
   end Enter_Root;
   
   --------------------
   -- Enter_All_AURA --
   --------------------
   
   procedure Enter_All_AURA is
      use Ada.Directories;
      
      AURA_Subsystem_Path: constant String := 
        Compose (Containing_Directory => Current_Directory,
                 Name                 => "aura");
   begin
      pragma Assert (Exists (AURA_Subsystem_Path)
                       and then Kind (AURA_Subsystem_Path) = Directory);
      
      -- Ensuring the 'aura' subdirectory exists is the responsibility of
      -- the Scheduling subsystem. This provides more ergonomic error
      -- reporting opportunities for the CLI, for what is possibly a common
      -- error
      
      Enter_Directory (Directory_Path => AURA_Subsystem_Path,
                       Order_Template => (AURA           => False,
                                          others         => <>));
   end Enter_All_AURA;
   
   ----------------------------
   -- Request_AURA_Subsystem --
   ----------------------------
   
   procedure Request_AURA_Subsystem (Name: in     Unit_Names.Unit_Name; 
                                     OK  :    out Boolean)
   is
      use Subsystems;
      package All_Subsystems renames Registry.All_Subsystems;
      
      Requested_Subsys: constant Subsystem
        := (AURA   => True,
            Name   => Name.Subsystem_Name,
            State  => Requested,
            others => <>);
      
   begin
      if All_Subsystems.Contains_Element (Requested_Subsys) then
         OK := All_Subsystems.Extract_Element(Requested_Subsys).AURA;
         -- The registry already contains this subsystem. If it is not AURA
         -- then that means the Root Project is already claiming this subsystem,
         -- and therefore we can't aquire it.
         --
         -- Otherwise it's already been requested or aquired, and so no action
         -- needs to be taken
         
      else
         -- Add it
         All_Subsystems.Insert (New_Item => Requested_Subsys,
                                Inserted => OK);
         
         pragma Assert (OK);
         -- There are no cases where OK should be False. First of all, we
         -- have already determined that it doesn't already exist in the set.
         -- So if OK is False, this should mean it was inserted sometime between
         -- that check and our attempt. That shouldn't happen because
         -- Request_AURA_Subsystem should only be called from the CLI driver
         -- before any work orders are executing.
      end if;
      
   end Request_AURA_Subsystem;
   
   ----------------------
   -- Update_Subsystem --
   ----------------------
   
   procedure Update_Subsystem (Update: Subsystems.Subsystem) is
   begin
      Registry.All_Subsystems.Replace (Update);
   end Update_Subsystem;
   
   -------------------------
   -- Update_Library_Unit --
   -------------------------
   
   procedure Update_Library_Unit (Update: in Library_Units.Library_Unit) is
   begin
      Registry.All_Library_Units.Replace (Update);
   end Update_Library_Unit;
   
   --------------------------------
   -- Update_Library_Unit_Subset --
   --------------------------------
   
   procedure Update_Library_Unit_Subset 
     (Update: in Library_Units.Library_Unit_Sets.Set) is
   begin
      if not Registry.All_Library_Units.Is_Subset (Update) then
         raise Constraint_Error with
           "Update_Library_Unit_Subset cannot add new units";
      end if;
      
      Registry.All_Library_Units.Include_Subset (Update);
   end Update_Library_Unit_Subset;
   
   -----------------------
   -- Exclude_Manifests --
   -----------------------
   
   procedure Exclude_Manifests is
      use Unit_Names;
      use Library_Units;
      
      Manifest: Library_Unit;
   begin
      for Subsys of Registry.All_Subsystems.Extract_Set loop
         Manifest.Name := Subsys.Name & ".aura";
         if Registry.All_Library_Units.Contains_Element (Manifest) then
            Manifest := Registry.All_Library_Units.Extract_Element (Manifest);
            Registrar.Registration.Unchecked_Deregister_Unit (Manifest);
         end if;
      end loop;
   end Exclude_Manifests;
   
   
end Registrar.Registration;
