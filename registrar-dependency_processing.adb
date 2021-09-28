------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
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

with Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Registrar.Queries;
with Registrar.Registry;
with Registrar.Subsystems;
with Registrar.Library_Units;
with Registrar.Registration.Unchecked_Deregister_Unit;
with Unit_Names, Unit_Names.Hash, Unit_Names.Sets;
with Workers, Workers.Reporting;

package body Registrar.Dependency_Processing is
   
   use type Unit_Names.Unit_Name;
   New_Line: Character renames Workers.Reporting.New_Line;
   
   --
   -- Common infrastructure
   --
   
   package UNQI is new Ada.Containers.Synchronized_Queue_Interfaces
     (Unit_Names.Unit_Name);
   
   package Name_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (UNQI);
   
   type Name_Queue_Access is access Name_Queues.Queue;
   
   type Reverse_Dependency_Queue is
      record
         Map_Key   : Unit_Names.Unit_Name;
         Name_Queue: Name_Queue_Access := null;
      end record;
   
   function Key_Hash (RDQ: Reverse_Dependency_Queue) 
                     return Ada.Containers.Hash_Type
     is (Unit_Names.Hash (RDQ.Map_Key));
   
   function Equivalent_Queues (Left, Right: Reverse_Dependency_Queue)
                              return Boolean
     is (Left.Map_Key = Right.Map_Key);
   
   package Map_Queue_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Reverse_Dependency_Queue,
      Hash                => Key_Hash,
      Equivalent_Elements => Equivalent_Queues);
   
   type Map_Queue_Set_Access is access Map_Queue_Sets.Set;
   
   --
   -- Work Orders Declarations
   --
   
   -----------------------------
   -- Phase 1: Merge Subunits --
   -----------------------------
   
   -- This phase is a bit of a "fake" one. It wouldn't be terribly efficient to
   -- parallelize this operation since the operations between each manipulation
   -- of the map are generally insignficant. The contention and scheduling
   -- overhead would likely far outstrip the performance gains of parallelizing
   -- it.
   
   type Merge_Subunits_Order is new Workers.Work_Order with
      record
         All_Units : Library_Units.Library_Unit_Sets.Set;
         All_Subsys: Subsystems.Subsystem_Sets.Set;
      end record;
   
   overriding function  Image   (Order: Merge_Subunits_Order) return String;
   overriding procedure Execute (Order: in out Merge_Subunits_Order);
   
   ----------------------
   -- Phase 2: Fan-out --
   ----------------------
   
   type Fan_Out_Order is new Workers.Work_Order with
      record
         Target: Unit_Names.Unit_Name;
         -- The unit that needs to be fanned-out
         
         Unit_Map_Queues  : Map_Queue_Set_Access;
         Subsys_Map_Queues: Map_Queue_Set_Access;
         -- Target deposits it's own name into the queue for each
         -- of its dependencies (unit and subsystem)
      end record;
   
   overriding function  Image         (Order: Fan_Out_Order) return String;
   overriding procedure Execute       (Order: in out Fan_Out_Order);
   overriding procedure Phase_Trigger (Order: in out Fan_Out_Order);
   
   ----------------------------
   -- Phase 3: Build Reverse --
   ----------------------------
   
   type Map_Domain is (Library_Units_Domain, Subsystems_Domain);
   -- Used to select which map is having its reverse dependency set
   -- build for -- Library_Units or Subsystems
   
   type Build_Reverse_Order is new Workers.Work_Order with
      record
         Domain      : Map_Domain;
         Queue_Cursor: Map_Queue_Sets.Cursor;

         -- The queue that should be used to build a reverse-dependency
         -- map for the unit/subsystem that "owns" the queue
         
         Unit_Map_Queues  : Map_Queue_Set_Access;
         Subsys_Map_Queues: Map_Queue_Set_Access;
         -- Both queues need to be referenced from all orders (hence not
         -- discriminated), since the Phase_Trigger needs to deallocate
         -- all
      end record;
   
   overriding function  Image  (Order: Build_Reverse_Order) return String;
   overriding procedure Execute       (Order: in out Build_Reverse_Order);
   overriding procedure Phase_Trigger (Order: in out Build_Reverse_Order);
   
   
   --
   -- Work Order Implementations
   --
   
   --------------------------
   -- Merge_Subunits_Order --
   --------------------------

   -- Image --
   -----------
   
   function Image (Order: Merge_Subunits_Order) return String is
     ("[Merge_Subunits_Order] (Registrar.Dependency_Processing)");
   
   -- Execute --
   -------------
   
   -- After doing our main job of merging subunit dependencies up to the
   -- parent, we also need to set-up the reverse depependency queues,
   -- and then dispatch Fan-Out orders (Phase 2)
   
   procedure Execute (Order: in out Merge_Subunits_Order) is
      use Subsystems;
      use Library_Units;
      use type Ada.Containers.Count_Type;
      
      Move_Set: Unit_Names.Sets.Set;
      
      Fan_Out: Fan_Out_Order;
      
      procedure Set_Extraction (Mod_Set: in out Unit_Names.Sets.Set) is
      begin
         Move_Set := Mod_Set;
         Mod_Set  := Unit_Names.Sets.Empty_Set;
      end Set_Extraction;
      
      procedure Merge_Extraction (Merge_Set: in out Unit_Names.Sets.Set) is
      begin
         Merge_Set.Union (Move_set);
      end Merge_Extraction;
   begin
      Fan_Out.Tracker           := Fan_Out_Progress'Access;
      Fan_Out.Unit_Map_Queues   := new Map_Queue_Sets.Set;
      Fan_Out.Subsys_Map_Queues := new Map_Queue_Sets.Set;
      
      for Unit of Order.All_Units loop
         
         if Unit.State = Requested then
            -- For Requested units - if we have them here it means none of the
            -- subsystems that were supposed to contain them actually do, or
            -- the entire subsystem could not be aquired.
            --
            -- We still want to compute the dependency maps so that we can give
            -- the user a picture of where the missing units were being withed
            -- from (similarily for subystems).
            --
            -- For those units, we need to add an empty set to the forward
            -- dependency map so that we don't need to check for No_Element
            -- cursors everywhere else. This makes particular sense considering
            -- this work order is single issue (not parallel), so there will be
            -- no contention on the dependency maps anyways.
            
            declare
               Inserted: Boolean;
            begin
               Registry.Unit_Forward_Dependencies.Insert
                 (Key      => Unit.Name, 
                  New_Item => Unit_Names.Sets.Empty_Set,
                  Inserted => Inserted);
               
               pragma Assert (Inserted);
            end;
            
            -- Subunits can't be requested! Every time a dependency request
            -- is processed by the registrar, it is entered as a library unit.
            -- I.e. a subsystem unit can only get into the registry by being
            -- entered, and therefore can't have a state of Requested.
            pragma Assert (Unit.Kind /= Subunit);
            
         end if;
         
         
         if Unit.Kind = Subunit then 
            -- Merge Subunit forward dependencies up to the fist non-subunit
            -- parent
            
            -- Take the forward dependency set from the subunit
            Registry.Unit_Forward_Dependencies.Modify 
              (Key     => Unit.Name,
               Process => Set_Extraction'Access);
            
            -- And migrate to its parent. This will keep happening until
            -- it reaches a unit that is not a subunit
            Registry.Unit_Forward_Dependencies.Modify
              (Key     => Registrar.Queries.Trace_Subunit_Parent(Unit).Name,
               Process => Merge_Extraction'Access);
         else
            -- A normal unit, which actually needs to be processesed. We want
            -- each dependency of this unit to have this unit registered as
            -- a reverse dependency for that dependent unit (this is what
            -- the Fan_Out order accomplishes).
            Fan_Out.Unit_Map_Queues.Insert 
              (Reverse_Dependency_Queue'(Map_Key    => Unit.Name,
                                         Name_Queue => new Name_Queues.Queue));
         end if;
         
         Merge_Subunits_Progress.Increment_Completed_Items;
      end loop;
      
      -- We set the Fan_Out progress tracker now, before we do the subsystems
      -- so that they are set before we cause the Merge_Subunits tracker to
      -- complete (we know there must be subsystems if there are units!).
      
      -- The number of Fan_Out orders is not dependent on the number of
      -- subsystems, since the subsystem dependencies are registered
      -- during the normal course of Fan_Out. Since all units have a subsystem,
      -- we can be sure all subsystem dependencies will be computed
      
      Fan_Out_Progress.Increase_Total_Items_By 
        (Natural (Fan_Out.Unit_Map_Queues.Length));
      
      for Subsys of Order.All_Subsys loop
         -- We don't really care what the state of the subsystem is, we just
         -- want to ensure that we have a queue for each subsystem we know
         -- about, so that the fan-out process can register subsystem
         -- dependencies
         declare
            Inserted: Boolean;
         begin
            Registry.Subsystem_Forward_Dependencies.Insert
              (Key      => Subsys.Name, 
               New_Item => Unit_Names.Sets.Empty_Set,
               Inserted => Inserted);
            
            pragma Assert (Inserted);
         end;
         
         Fan_Out.Subsys_Map_Queues.Insert 
           (Reverse_Dependency_Queue'(Map_Key    => Subsys.Name,
                                      Name_Queue => new Name_Queues.Queue));
         
         Merge_Subunits_Progress.Increment_Completed_Items;
      end loop;
      

      
      -- Launch the fan-out phase. We only need to launch an order per
      -- unit (not subsystem), since each unit dependency will the related
      -- subsystem dependency to the relevent subsystem map queue.
      
      for Queue of Fan_Out.Unit_Map_Queues.all loop
         Fan_Out.Target := Queue.Map_Key;
         Workers.Enqueue_Order (Fan_Out);
      end loop;
   end Execute;
   
   -------------------
   -- Fan_Out_Order --
   -------------------
   
   -- Image --
   -----------
   
   function Image (Order: Fan_Out_Order) return String is
     (    "[Fan_Out_Order] (Registrar.Dependency_Processing)" & New_Line
        & " Target Unit: " & Order.Target.To_UTF8_String);
   
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Fan_Out_Order) is
      use type Map_Queue_Sets.Cursor;
      use type Library_Units.Library_Unit_Kind;
      
      Dependencies: constant Unit_Names.Sets.Set
        := Registry.Unit_Forward_Dependencies.Extract_Element (Order.Target);
      
      Subsys_Dependencies: Unit_Names.Sets.Set;
      -- We will also build the subsystem forward dependencies for the unit's
      -- subsystem as we go here (which will then be reversed in the
      -- Build Reverse phase
      
      Target_SS: constant Unit_Names.Unit_Name := Order.Target.Subsystem_Name;
      
      Unit_Map_Queues  : Map_Queue_Sets.Set renames Order.Unit_Map_Queues.all;
      Subsys_Map_Queues: Map_Queue_Sets.Set renames Order.Subsys_Map_Queues.all;
      
      Unit_Lookup, Subsys_Lookup: Reverse_Dependency_Queue;
      Unit_Cursor, Subsys_Cursor: Map_Queue_Sets.Cursor;
      
      procedure Union_Subsys_Dependencies
        (Existing_Set: in out Unit_Names.Sets.Set)
      is begin
         Existing_Set.Union (Subsys_Dependencies);
      end Union_Subsys_Dependencies;
      
   begin
      -- Go through our (Order.Target's) dependencies and put our name on the
      -- reverse dependency queue for that unit.
      
      for Name of Dependencies loop
         -- Add the name of our target unit, and our subsystem 
         -- to the queue of each of our dependencies
         Unit_Lookup.Map_Key   := Name;
         Subsys_Lookup.Map_Key := Name.Subsystem_Name;
         Subsys_Dependencies.Include (Subsys_Lookup.Map_Key);
         
         Unit_Cursor   := Order.Unit_Map_Queues.Find   (Unit_Lookup);
         Subsys_Cursor := Order.Subsys_Map_Queues.Find (Subsys_Lookup);
                                             
         Unit_Map_Queues(Unit_Cursor).Name_Queue.Enqueue (Order.Target);
         Subsys_Map_Queues(Subsys_Cursor).Name_Queue.Enqueue (Target_SS);
      end loop;
      
      -- Add a link back to our parent, except for External_Units, but
      -- including requested units
      
      if Registrar.Queries.Lookup_Unit(Order.Target).Kind 
        /= Library_Units.External_Unit 
      then
         Unit_Lookup.Map_Key := Order.Target.Parent_Name;
         if not Unit_Lookup.Map_Key.Empty then
            Unit_Cursor := Unit_Map_Queues.Find (Unit_Lookup);
            Unit_Map_Queues(Unit_Cursor).Name_Queue.Enqueue (Order.Target);
         end if;
      end if;
      
      -- Register the subsystem forward dependencies. We know that the Key
      -- exists because we added an empty set for each registered Subsystem
      
      Registry.Subsystem_Forward_Dependencies.Modify
        (Key     => Target_SS,
         Process => Union_Subsys_Dependencies'Access);
      
   end Execute;
   
   -- Phase_Trigger --
   -------------------
   
   procedure Phase_Trigger (Order: in out Fan_Out_Order) is
      New_Order: Build_Reverse_Order
        := (Tracker           => Build_Reverse_Progress'Access,
            Domain            => Library_Units_Domain,
            Unit_Map_Queues   => Order.Unit_Map_Queues,
            Subsys_Map_Queues => Order.Subsys_Map_Queues,
            others            => <>);
      
      use type Ada.Containers.Count_Type;
   begin
      Build_Reverse_Progress.Increase_Total_Items_By 
        (Natural (Order.Unit_Map_Queues.Length 
                    + Order.Subsys_Map_Queues.Length));
      
      for Unit_Cursor in Order.Unit_Map_Queues.Iterate loop
         New_Order.Queue_Cursor := Unit_Cursor;
         Workers.Enqueue_Order (New_Order);
      end loop;
      
      New_Order.Domain := Subsystems_Domain;
      
      for Subsys_Cursor in Order.Subsys_Map_Queues.Iterate loop
         New_Order.Queue_Cursor := Subsys_Cursor;
         Workers.Enqueue_Order (New_Order);
      end loop;
      
   end Phase_Trigger;
   
   -------------------------
   -- Build_Reverse_Order --
   -------------------------
   
   -- Image --
   -----------
   
   function Image (Order: Build_Reverse_Order) return String is
     (  "[Build_Reverse_Order] (Registrar.Dependency_Processing)" 
        & New_Line
        & (if Map_Queue_Sets."="
             (Order.Queue_Cursor, Map_Queue_Sets.No_Element) 
           then
              " ** Phase Trigger **"
              -- If something goes wrong in the phase trigger, either
              -- of the queues might be already deallocated, which would
              -- make the "else" portion very erronious to execute
           else
              " Domain: " & Map_Domain'Image (Order.Domain)
              & New_Line
              & (case  Order.Domain is
                    when Library_Units_Domain => "Unit Queue: " 
                         & Order.Unit_Map_Queues.all 
                           (Order.Queue_Cursor).Map_Key.To_UTF8_String,
                    when Subsystems_Domain => "Subsystem Queue: "
                         & Order.Subsys_Map_Queues.all
                           (Order.Queue_Cursor).Map_Key.To_UTF8_String)));
   
   -- Note that if an exception is raised during the phase trigger,
   -- it is possible t hat this 
   
   
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Build_Reverse_Order) is
      use type Ada.Containers.Count_Type;
      
      Queue_Set: constant Map_Queue_Set_Access 
        := (case Order.Domain is
               when Library_Units_Domain => Order.Unit_Map_Queues,
               when Subsystems_Domain    => Order.Subsys_Map_Queues);
      
      Queue_Descriptor: Reverse_Dependency_Queue
        renames Queue_Set.all(Order.Queue_Cursor);
      
      Queue: Name_Queues.Queue renames Queue_Descriptor.Name_Queue.all;
      
      Rev_Deps: Unit_Names.Sets.Set;
      Name: Unit_Names.Unit_Name;
      
      procedure Include_Reverse_Dependencies (Set: in out Unit_Names.Sets.Set) 
      is begin
         -- Thge target set already has a map for this unit, so we simply
         -- union-in the names we have.
         
         Set.Union (Rev_Deps);
      end Include_Reverse_Dependencies;
   begin
      while Queue.Current_Use > 0 loop
         -- Note that in this phase (phase "3", each queue is assigned to a
         -- single Work Order, and therefore we will not see concurrent access
         -- to our queue from this Order
         
         Queue.Dequeue (Name);
         Rev_Deps.Include (Name); 
         -- The same name will likely appear twice.
      end loop;
      
      case Order.Domain is
         when Library_Units_Domain =>
            Registry.Unit_Reverse_Dependencies.Insert_Or_Modify
              (Key              => Queue_Descriptor.Map_Key,
               New_Item         => Rev_Deps,
               Process_Existing => Include_Reverse_Dependencies'Access);
            
         when Subsystems_Domain =>
            Registry.Subsystem_Reverse_Dependencies.Insert_Or_Modify
              (Key              => Queue_Descriptor.Map_Key,
               New_Item         => Rev_Deps,
               Process_Existing => Include_Reverse_Dependencies'Access);
      end case;
      
      -- Finalize the order's Queue_Cursor element. This is important because
      -- the Phase trigger might complete before all of the "completed orders"
      -- are actually finalized. This would cause a Tampering with Cursors
      -- check to fail.
      
      Order.Queue_Cursor := Map_Queue_Sets.No_Element;
      
   end Execute;
   
   -- Phase_Trigger --
   -------------------
   
   procedure Phase_Trigger (Order: in out Build_Reverse_Order) is
      
      procedure Free_Queue is new Ada.Unchecked_Deallocation
        (Object => Name_Queues.Queue, Name => Name_Queue_Access);
      
      procedure Free_Queue_Set is new Ada.Unchecked_Deallocation
        (Object => Map_Queue_Sets.Set, Name => Map_Queue_Set_Access);
      
      Queue_Disposal: Name_Queue_Access;
      -- Since hash map only have constant referencing, we'll copy out
      -- the access value for the queue in order to deallocate it
      
   begin
      pragma Assert (Map_Queue_Sets."=" (Order.Queue_Cursor,
                                         Map_Queue_Sets.No_Element));
      
      -- The phase trigger just needs to clean-up the allocated objects.
      -- This is nice because the Consolodate_Dependencies process is now
      -- finished and everything else can proceed while this worker deals
      -- with this clean-up.
      --
      -- This is why we didn't deallocate the queues during Execute.
      
      for QD of Order.Unit_Map_Queues.all loop
         Queue_Disposal := QD.Name_Queue;
         Free_Queue (Queue_Disposal);
      end loop;
      
      Free_Queue_Set (Order.Unit_Map_Queues);
      
      for QD of Order.Subsys_Map_Queues.all loop
         Queue_Disposal := QD.Name_Queue;
         Free_Queue (Queue_Disposal);
      end loop;
      
      Free_Queue_Set (Order.Subsys_Map_Queues);
      
   end Phase_Trigger;
   
   --
   -- Dispatch
   -- 
   
   ------------------------------
   -- Consolidate_Dependencies --
   ------------------------------
   
   procedure Consolidate_Dependencies is
      use type Ada.Containers.Count_Type;
      
      Order: Merge_Subunits_Order;
   begin
      Order.Tracker   := null; -- Only one order, and the tracker is manually
                               -- adjusted
      Order.All_Units  := Registrar.Queries.All_Library_Units;
      Order.All_Subsys := Registrar.Queries.All_Subsystems;
      Merge_Subunits_Progress.Increase_Total_Items_By 
        (Natural (Order.All_Units.Length + Order.All_Subsys.Length));
      Workers.Enqueue_Order (Order);
   end Consolidate_Dependencies;
   
end Registrar.Dependency_Processing;
