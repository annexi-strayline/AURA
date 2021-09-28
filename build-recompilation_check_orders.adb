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

with Ada.Unchecked_Deallocation;

with Stream_Hashing;
with Registrar.Registration;

separate (Build)

package body Recompilation_Check_Orders is
   
   package Reg_Qs renames Registrar.Queries;
   use type Stream_Hashing.Hash_Type;
   use all type Registrar.Library_Units.Library_Unit_Kind;
   
   -------------------
   -- Test_Isolated --
   -------------------
   
   procedure Test_Isolated 
     (Unit    : in out Registrar.Library_Units.Library_Unit;
      Isolated:    out Boolean)
   with Pre => Unit.Kind in Package_Unit | Subprogram_Unit;
   
   -- Scans the specification of Unit for any generic declarations or Inline
   -- subprograms. If any are found, Isolated is set to False, otherwise
   -- Isolated is set to True.
   --
   -- Note that this scan is somepessemistic. There may be some cases which can
   -- cause it to "wrongly" classify a unit as being NOT isolated when
   -- it really is (false negative). It shouldn't have false positives,
   -- however.
   --
   -- One interesting case is where a generic unit is declared in the private
   -- part of a package. In this case only some of the dependent units would
   -- need to be recompiled. Test_Isolated will not bother trying to sort that
   -- out.
   
   procedure Test_Isolated 
     (Unit    : in out Registrar.Library_Units.Library_Unit;
      Isolated:    out Boolean)
   is separate;
   
   -----------------------
   -- Recompilation_Set --
   -----------------------
   
   protected body Recompilation_Set is
      
      ------------------
      -- Enter_Subset --
      ------------------
      
      procedure Enter_Subset (Entry_Subset: in out Unit_Names.Sets.Set) is
      begin
         Entry_Subset.Difference (Master);
         Master.Union (Entry_Subset);
      end Enter_Subset;
      
      function Retrieve return Unit_Names.Sets.Set is (Master);
   end Recompilation_Set;
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Recompilation_Check_Order) return String is
     (    "[Recompilation_Check_Order] (Build)" & New_Line
        & " Target: " & Order.Target.To_UTF8_String & New_Line
        & " Mode  : " & Processing_Mode'Image (Order.Mode) & New_Line);
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Recompilation_Check_Order) is
      use Registrar.Library_Units;
      
      package Last_Run renames Registrar.Last_Run;
      
      function Seek_Parent (Subunit_Name: Unit_Names.Unit_Name) 
                           return Unit_Names.Unit_Name is
        (Reg_Qs.Trace_Subunit_Parent(Reg_Qs.Lookup_Unit (Subunit_Name)).Name);
      
      procedure Recompile_All is
         -- Recompile_All recursively triggers the recompilation of the
         -- Target unit, as well as all reverse dependencies of that unit
         
         Recurse_Order: Recompilation_Check_Order 
           := (Tracker    => Order.Tracker,
               Mode       => Set,
               Recomp_Set => Order.Recomp_Set,
               others     => <>);
         -- A delta aggregate would be nice here..
         
         Rev_Deps: Unit_Names.Sets.Set
           := Registrar.Queries.Dependent_Units (Order.Target);
         
      begin
         pragma Assert 
           (for all Unit of Rev_Deps 
              => Reg_Qs.Lookup_Unit(Unit).Kind /= Subunit);
         -- This should be managed in the Registrar.Dependency_Processing.
         -- Consolidate_Dependencies operation.
         
         -- For Test modes, add Target directly to the Recomp_Set since Test
         -- mode indicates that Target is the root of a dependency tree, and so
         -- unlike Set orders, has not already been added by an earlier
         -- recursive order
         
         if Order.Mode = Test then
            declare
               Just_Me: Unit_Names.Sets.Set 
                 := Unit_Names.Sets.To_Set (Order.Target);
               -- Needs to be a variable for Enter_Subset
            begin
               Order.Recomp_Set.Enter_Subset (Just_Me);
            end;
         end if;
            
         Order.Recomp_Set.Enter_Subset (Rev_Deps);
         
         -- Now Rev_Deps only has the unit names that have not already
         -- been marked as needing recompilation (the units that have
         -- just been added to the Recomp_Set. So we need to dispatch
         -- recursive orders for them, so that they will have their
         -- dependencies added aswell
         
         Recurse_Order.Tracker.Increase_Total_Items_By
           (Natural (Rev_Deps.Length));
         
         for Name of Rev_Deps loop
            Recurse_Order.Target := Name;
            Workers.Enqueue_Order (Recurse_Order);
         end loop;
         
      end Recompile_All;
      
      This, Last: Library_Unit;
      Isolated  : Boolean;
      
   begin
      case Order.Mode is
         when Test =>
            pragma Assert (not Last_Run.All_Library_Units.Is_Empty);
            -- Build.Compute_Recompilations is not supposed to submit any work
            -- orders at all if there is no Last_Run registry
            
            This := Reg_Qs.Lookup_Unit (Order.Target);
            
            pragma Assert (This.State = Compiled);
            -- We should only ever be "testing" units that are compiled.
            
            Last := Last_Run.All_Library_Units 
              (Last_Run.All_Library_Units.Find (This));
            -- Note the lookup of Last should not fail because we'd only get
            -- here if This had a state of "Compiled", and therefore it
            -- must be in Last. If this fails, good, something is very wrong.
            
            if        Last.State /= Compiled
              or else This.Kind /= Last.Kind
              or else (if This.Spec_File = null then
                          This.Implementation_Hash /= Last.Implementation_Hash
                       else
                          This.Specification_Hash /= Last.Specification_Hash)
              -- Remember that subprogram units do not need a separate spec,
              -- so if the body changes for such a subprogram, we cannot assume
              -- that it doesn't effect it's reverse dependencies
              
              or else This.Compilation_Hash /= Last.Compilation_Hash
            then
               -- Definately needs recompilation
               Recompile_All;
               
            elsif This.Implementation_Hash /= Last.Implementation_Hash then
               pragma Assert (This.Body_File /= null);
               
               if This.Kind not in 
                 Package_Unit | Subprogram_Unit | Subunit 
               then
                  -- All non-Ada units are assumed to not be isolated.
                  -- Also, Test_Isolated is expecting an Ada source
                  Isolated := False;
               else
                  Test_Isolated (Unit => This, Isolated => Isolated);
               end if;
               
               if Isolated then
                  -- This unit can apparently be recompiled on its own
                  declare
                     Just_Me: Unit_Names.Sets.Set 
                       := Unit_Names.Sets.To_Set (Order.Target);
                  begin
                     Order.Recomp_Set.Enter_Subset (Just_Me);
                  end;
               else
                  -- No luck
                  Recompile_All;
               end if;
            end if;
              
         when Set =>
            Recompile_All;
            
      end case;
   end Execute;
   
   -------------------
   -- Phase_Trigger --
   -------------------
   
   procedure Phase_Trigger (Order: in out Recompilation_Check_Order) is
      use Registrar.Library_Units;
      
      -- Now the Recomp_Set should be complete. We'll use it to pull a
      -- subset of All_Library_Units, and set all of the states to
      -- "Available"
      
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Recompilation_Set,
         Name   => Recompilation_Set_Access);
      
      Selected_Units: Library_Unit_Sets.Set := Reg_Qs.Entered_Library_Units;
      Recomp_Units  : Library_Unit_Sets.Set;
      
   begin
      for Name of Order.Recomp_Set.Retrieve loop
         Recomp_Units.Include (Library_Unit'(Name => Name, others => <>));
      end loop;
      
      Free (Order.Recomp_Set);
      Selected_Units.Intersection (Recomp_Units);
      
      declare
         procedure Set_Available (Unit: in out Library_Unit) is
         begin
            Unit.State := Available;
         end Set_Available;
      begin
         for C in Selected_Units.Iterate loop
            Library_Unit_Sets_Keyed_Operations.Update_Element_Preserving_Key
              (Container => Selected_Units,
               Position  => C,
               Process   => Set_Available'Access);
         end loop;
      end;
      
      Registrar.Registration.Update_Library_Unit_Subset (Selected_Units);
      Compute_Recompilations_Completion.Leave;
      
   exception
      when others =>
         Free (Order.Recomp_Set);
         if Compute_Recompilations_Completion.Active then
            Compute_Recompilations_Completion.Leave;
         end if;
         
         raise;
   end Phase_Trigger;
   
end Recompilation_Check_Orders;
