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

with Ada.Assertions;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Unit_Names;
with Workers, Workers.Reporting;
with Registrar.Queries;
with Registrar.Registry;
with Registrar.Source_Files;
with Registrar.Subsystems;
with Stream_Hashing.Collective;

package body Registrar.Implementation_Hashing is
   
   use type Ada.Containers.Count_Type;
   use type Library_Units.Library_Unit_Sets.Cursor;
   use type Registrar.Source_Files.Source_File_Access;
   
   --
   -- Guard_Beacon
   --
   
   Guard_Beacon: Progress.Sequence_Beacon;
   -- Approached at the start of every hashing pass, left at start of the phase
   -- trigger of the crunch phase
   --
   -- The Guard_Beacon ensures that the phase trackers which are reponsible
   -- for phase triggers (Collection_Phase_Progress and Crunch_Phase_Progress)
   -- are not clobbered by simiultaneous runs of Hash_Subset (there shouldn't
   -- be any of those anyways)

   
   --
   -- Collection Sets
   --
   
   -- Only actual library units need to have an implementation hash. Many
   -- library units will have both a specification and a body, as well as
   -- any number of subunit bodies. This means that there are often multiple
   -- hashes that need to end up in a single Library_Unit object. To facilitate
   -- parallelism, we want to be able to give each individual hash a target
   -- of sorts. Each target refers to a single library unit, and contains a
   -- synchronized queue to receive the various hashes. This target is the
   -- Collection_Unit.
   
   package Hash_Queues renames Stream_Hashing.Collective.Hash_Queues;
   type Hash_Queue_Access is access Hash_Queues.Queue;
   type Library_Unit_Set_Access is access Library_Units.Library_Unit_Sets.Set;
   
   procedure Free_Library_Units_Set is new Ada.Unchecked_Deallocation
     (Object => Library_Units.Library_Unit_Sets.Set,
      Name   => Library_Unit_Set_Access);
   
   ---------------------
   -- Collection_Unit --
   ---------------------
   
   type Collection_Unit is
      record
         Target_Unit: Library_Units.Library_Unit_Sets.Cursor;
         Hash_Queue : Hash_Queue_Access;
      end record;
   
   ---------------------
   -- Collection_Sets --
   ---------------------
   
   function Collection_Unit_Hash (CU: Collection_Unit) 
                                 return Ada.Containers.Hash_Type
   is 
      use Library_Units;
      Hash: Ada.Containers.Hash_Type;
      
      procedure Get_Hash (Element: in Library_Unit) is
      begin
         Hash := Library_Unit_Name_Hash (Element);
      end Get_Hash;
      
   begin
      Library_Unit_Sets.Query_Element (Position => CU.Target_Unit,
                                       Process  => Get_Hash'Access);
      return Hash;
   end Collection_Unit_Hash;
   
   function Equivalent_Units (Left, Right: Collection_Unit) return Boolean is
     (Left.Target_Unit = Right.Target_Unit);
   
   -- GNAT BUG
   --
   -- Note that there is (was) a bug in GNAT where hashed container cursors
   -- could be generate cursors that designate the same object but which do
   -- not evaluate to equal. This was because the cursor types contained an
   -- extra component that was used lazily for opimization. Since the RM
   -- requires the cursor's predefined equality to evaluate true whenever
   -- the designated object is the same for both cursors, this violated this
   -- requirement. This bug therefore can (and does) cause Equivalent_Units
   -- to evaluate False when it should Evaluate True. It just so happens that
   -- this particular case makes this bug benign. The effect is ultimately
   -- unnecessary recompilations. For that reason, it was decided to leave it
   -- as-is.
   --
   -- A patch to fix this bug was submitted by us (ANNEXI-STRAYLINE), and will
   -- likely appear in GCC 11.
   
   package Collection_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Collection_Unit,
      Hash                => Collection_Unit_Hash,
      Equivalent_Elements => Equivalent_Units);
   
   type Collection_Set_Access is access Collection_Sets.Set;

      
   procedure Free_Collection_Set is new Ada.Unchecked_Deallocation
     (Object => Collection_Sets.Set,
      Name   => Collection_Set_Access);
   
   --
   -- Phase Implementations
   --
   
   New_Line: Character renames Workers.Reporting.New_Line;
   
   -- Collection Phase --
   ----------------------
   
   -- The Collection_Order takes a hash queue and a cursor to
   -- a Library_Unit. The worker than enqueues the Body_File hash,
   -- as well as all subunit bodies onto the hash queue.
   --
   -- A Phase_Trigger iterates over Process_Set, submitting a Crunch_Order
   -- for each Collection_Unit
   
   package Collection_Orders is
      
      type Collection_Order is new Workers.Work_Order with
         record
            Source        : Library_Units.Library_Unit_Sets.Cursor;
            Source_Set    : Library_Unit_Set_Access;
            
            Collector     : Collection_Sets.Cursor;
            Collection_Set: Collection_Set_Access;
         end record;
      
      overriding procedure Execute       (Order: in out Collection_Order);
      overriding procedure Phase_Trigger (Order: in out Collection_Order);
      overriding function  Image  (Order: Collection_Order) return String;
      
   end Collection_Orders;
   
   
   -- Crunch Phase --
   ------------------
      
   -- The Crunch_Collection phase is dispatched from the last
   -- Collect_Hash_Order's Phase_Trigger, one for each Collection_Unit,
   -- and then deallocates the queue
   
   package Crunch_Orders is
      
      type Crunch_Order is new Workers.Work_Order with
         record
            Source_Set    : Library_Unit_Set_Access;
            
            Collector     : Collection_Sets.Cursor;
            Collection_Set: Collection_Set_Access;
         end record;
      
      overriding procedure Execute       (Order: in out Crunch_Order);
      overriding procedure Phase_Trigger (Order: in out Crunch_Order);
      overriding function  Image  (Order: Crunch_Order) return String;
      
   end Crunch_Orders;
   

   -- Cleanup Order --
   -------------------
   
   -- The Cleanup Order Phase is always a single work order that is dispatched
   -- from the phase trigger of the last Crunch_Order.
   --
   -- The Cleanup order deallocates the working subset
   
   type Cleanup_Order is new Workers.Work_Order with
      record
         Source_Set    : Library_Unit_Set_Access;
         Collection_Set: Collection_Set_Access;
      end record;
   
   overriding procedure Execute (Order: in out Cleanup_Order);
   overriding function  Image   (Order: Cleanup_Order) return String;
   
   
   package body Collection_Orders is separate;
   package body Crunch_Orders     is separate;
   
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Cleanup_Order) return String is
     ("[Cleanup_Order] (Registrar.Implementation_Hashing)");
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Cleanup_Order) is
   begin
      Free_Collection_Set    (Order.Collection_Set);
      Free_Library_Units_Set (Order.Source_Set);
   end Execute;
   
   --
   -- Package Interface
   --
   
   procedure Hash_Subset
     (Reference_Subset: Library_Units.Library_Unit_Sets.Set);
   
   
   --------------
   -- Hash_All --
   --------------
   
   -- Filter everything except requested units
   use type Library_Units.Library_Unit_State;
   
   function All_Not_Requested (Unit: Library_Units.Library_Unit) 
                              return Boolean is 
     (Unit.State /= Library_Units.Requested);
   
   --------------------------------------------------
   procedure Hash_All is
   begin
      Hash_Subset (Registry.All_Library_Units.Extract_Subset 
                     (All_Not_Requested'Access));
   end Hash_All;
   
   -------------------------
   -- Hash_Configurations --
   -------------------------
   
   procedure Hash_Configurations is
      use Subsystems;
      use Library_Units;
      use type Unit_Names.Unit_Name;
      
      function Filter_Aquired (SS: Subsystem) return Boolean
        is (SS.State = Aquired);
      
      Aquired_SS: constant Subsystem_Sets.Set
        := Registry.All_Subsystems.Extract_Subset (Filter_Aquired'Access);
      
      Check_Set: Library_Unit_Sets.Set;
      
   begin
      if Aquired_SS.Is_Empty then
         -- Nothing to do anyways
         return;
      end if;
      
      -- Build the Check_Set, with the unit names of the manifest and
      -- configuration unit of each subsystem in the Aquired_SS set
      for Subsys of Aquired_SS loop
         -- Manifest
         Check_Set.Insert 
           (Library_Unit'(Name   => Subsys.Name & ".aura",
                          others => <>));
         -- Config unit
         Check_Set.Insert 
           (Library_Unit'(Name   => "aura." & Subsys.Name,
                          others => <>));
      end loop;
      
      -- We now take an intersection of All_Library_Units and send that off
      -- to Hash_Subset. This will be a set of all units of All_Library_Units
      -- that is equivalent to Check_Set
      
      Hash_Subset 
        (Library_Units.Library_Unit_Sets.Intersection 
           (Left  => Registry.All_Library_Units.Extract_Subset
              (All_Not_Requested'Access),
            Right => Check_Set));
      
   end Hash_Configurations;
   
   -----------------
   -- Hash_Subset --
   -----------------
   
   procedure Hash_Subset
     (Reference_Subset: Library_Units.Library_Unit_Sets.Set)

   is
      use Library_Units;
      
      package Order_Prequeues is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Collection_Orders.Collection_Order,
         "="          => Collection_ORders."=");
      
      Order: Collection_Orders.Collection_Order;
      Prequeue: Order_Prequeues.Vector;
      
      Subset    : Library_Unit_Set_Access;
      Collectors: Collection_Set_Access;
      
      Exec_Go: Boolean;
      
      procedure Include_Collector 
        (For_Unit: in     Library_Unit_Sets.Cursor;
         Position:    out Collection_Sets.Cursor) 
      is
         use type Collection_Sets.Cursor;
         
         New_Collector: Collection_Unit 
           := (Target_Unit => For_Unit, Hash_Queue => null);
         
         Inserted: Boolean;
      begin
         pragma Assert (Subset.all(For_Unit).Kind /= Subunit);
         Position := Collectors.Find (New_Collector);
         
         if Position = Collection_Sets.No_Element then
            New_Collector.Hash_Queue := new Hash_Queues.Queue;
            Collectors.Insert (New_Item => New_Collector,
                               Position => Position,
                               Inserted => Inserted);
            
            pragma Assert (Inserted);
            -- This is really an assumption, and could only fail if
            -- there was somehow concurrent access
         end if;
      end Include_Collector;
      
   begin
      if Reference_Subset.Length = 0 then
         return;
      end if;
      
      -- Ensure that any previous implementation hashing operations have
      -- fully completed before trying again.
      
      Guard_Beacon.Wait_Leave;
      Guard_Beacon.Approach (Exec_Go);
      
      if not Exec_Go then
         raise Program_Error with
           "Simultaneous implementation hashing passes";
      end if;
      
      Order.Tracker := Collection_Phase_Progress'Access;
      
      if Reference_Subset.Length = 0 then
         return;
      end if;
      
      Subset     := new Library_Unit_Sets.Set'(Reference_Subset);
      Collectors := new Collection_Sets.Set;
      
      Order.Source_Set     := Subset;
      Order.Collection_Set := Collectors;
      
      for Unit_Cursor in Subset.Iterate loop
         Order.Source := Unit_Cursor;
         
         case Subset.all(Unit_Cursor).Kind is
            when Package_Unit | Subprogram_Unit | External_Unit =>
               -- It's possible that a subunit of this unit has been
               -- encountered already, and therefore this unit has
               -- already been entered into Collectors, hence "include"
               -- instead of "insert".
               
               Include_Collector (For_Unit => Unit_Cursor,
                                  Position => Order.Collector);
               
            when Subunit =>
               -- For subunits, we need to target the parent that is
               -- in the Subset and that is not a Subunit. If
               -- this unit has not already been added, we add it
               -- preemptively
               
               -- These "subunits" likely have their own subunit
               -- bodies, which need to be included in the hash as well.
               
               declare
                  Parent_Cursor: Library_Units.Library_Unit_Sets.Cursor
                    := Subset.Find 
                      (Registrar.Queries.Trace_Subunit_Parent
                         (Subset.all(Unit_Cursor)));
                  
               begin
                  pragma Assert (Parent_Cursor /= Library_Unit_Sets.No_Element);
                  -- There should not be such a subset submitted that does
                  -- not also contain the parents of all subunits.
                  
                  Include_Collector (For_Unit => Parent_Cursor,
                                     Position => Order.Collector);
               end;
               
            when Unknown =>
               -- This should not appear here!
               raise Program_Error 
                 with "Unknown unit kind should not be hashed";
         end case;
         
         Prequeue.Append (Order);
         
      end loop;
      
      -- Now we can preset the trackers. We need to do this after the previous
      -- run because we want to be able to set the Crunch_Phase tracker before
      -- we submit the last order, so that the caller doesn't prematurely skip
      -- that tracker
      
      Collection_Phase_Progress.Increase_Total_Items_By 
        (Natural (Prequeue.Length));
      
      Crunch_Phase_Progress.Increase_Total_Items_By
        (Natural (Collectors.Length));
      
      -- Dispatch
      
      for PQO of Prequeue loop
         Workers.Enqueue_Order (PQO);
      end loop;
      
      -- Note that any exception in this subprogram will be fatal for the whole
      -- AURA operation, and so there is not much value to complex error
      -- recovery (particularily for reclaiming dynamic allocations)
      
   end Hash_Subset;
   
end Registrar.Implementation_Hashing;
