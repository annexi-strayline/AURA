------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
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
with Ada.Containers;

with Registrar.Registry;
with Registrar.Source_Files;

package body Registrar.Executive.Library_Unit_Registration is
   
   use type Ada.Containers.Count_Type;
   
   -----------
   -- Image --
   -----------
   
   overriding 
   function Image (Order: Library_Unit_Registration_Order)
                  return String
     is (  "[Library_Unit_Registration_Order]" & New_Line &
           "AURA          : " & Boolean'Image (Order.AURA) &  New_Line &
           "Registrant    : " & Order.Registrant.Name.To_UTF8_String 
           & New_Line &
           (if Order.AURA then 
               "AURA_Subsystem: " & Order.AURA_Subsystem.Name.To_UTF8_String
            else
               "") & New_Line &
           "Subunit Bodies:"
           & Ada.Containers.Count_Type'Image 
             (Order.Registrant.Subunit_Bodies.Length));
   
   -------------
   -- Execute --
   -------------
   
   overriding 
   procedure Execute (Order: in out Library_Unit_Registration_Order) is
      use Ada.Assertions;
      
      use Unit_Names;
      use Registrar.Library_Units;
      use Registrar.Subsystems;
      
      use type Registrar.Source_Files.Source_File_Access;
      
      package All_Library_Units renames Registrar.Registry.All_Library_Units;
      package All_Subsystems    renames Registrar.Registry.All_Subsystems;
      
      New_Unit: Library_Unit renames Order.Registrant;
      
      
      procedure Modify_Unit (Existing_Unit: in out Library_Unit) is
      begin
         -- Attempt to correctly modify the existing element.
         -- We expect that any non-null files of ths incoming unit will match
         -- with null files of the existing unit.
         
         -- We should also make sure that the state progresses to Registered
         -- (since that is really the role of unit registration in the first-
         -- place. Units that have a body but no spec will be delt with at a
         -- later time
         
         Assert (Check   => Existing_Unit.State /= Compiled,
                 Message => "Addition of item to an already compiled unit");
         
         -- Note that a new registration never has both a body and a spec
         -- at the same time, so we can increase efficincy via a bit by
         -- using if + elsif instead of two ifs
         
         pragma Assert (if New_Unit.Spec_File = New_Unit.Body_File then
                          (New_Unit.Spec_File = null
                             and then New_Unit.Subunit_Bodies.Length > 0)
                        else
                          ((New_Unit.Spec_File = null) 
                             = (New_Unit.Body_File /= null)));
         
         -- If a subunit is registered before the associated library unit is
         -- registered (particularily if it hasn't been requested rist), that
         -- library unit will get a placeholder - essentially it was assumed
         -- that the subunit was associated (nested in) another subunit.
         --
         -- In these cases, the New_Unit, which is registering a library unit,
         -- might have a subunit in place. We should therefore set the Kind
         -- in this case, while we're at it, we should also ensure that the
         -- New_Unit's kind matches the Existing_Unit's kind where (and only
         -- where) the existing unit is not of Kind "Subunit"
         
         pragma Assert (New_Unit.Kind /= Unknown);
         
         case Existing_Unit.Kind is
            
            when Unknown =>
               -- We're registering a requested unit, which is basically a
               -- place-holder.
               
               -- We don't really want to go whole-sale replacing of the
               -- Unit, because it breaks some of the continuity with the
               -- code that follows this case statement. Also, doing it
               -- surgically like this might be slightly more performant.
               
               -- We first set the kind, and ensure that we transfer any
               -- of the subunit bodies
               
               Existing_Unit.Kind           := New_Unit.Kind;
               Existing_Unit.Subunit_Bodies := New_Unit.Subunit_Bodies;
               
            when Subunit =>
               -- If the existing unit is a Subunit, but the inbound one isn't,
               -- just means that we've already registered subunits for the
               -- unit, but haven't seen the actual parent until now, so
               -- obviously we want to set the parent to the appropriate kind
               
               -- Note that a unit registered as a "Subunit" is a subunit
               -- itself, and must therefore not have a spec or a body file.
               -- The incoming unit shall have one of those
               
               pragma Assert (New_Unit.Kind /= External_Unit);
               
               Existing_Unit.Kind := New_Unit.Kind;
               
            when Package_Unit | Subprogram_Unit =>
               
               -- So the existing unit is either the spec or the body, so we
               -- expect to have either/or coming in and we expect that both
               -- have the same Kind. Otherwise the new "unit" should be a
               -- subunit assigned to the existring unit. 
               -- These need to be checked with a hard assertion, since the
               -- environment has potential to produce violations here
               
               Assert (Check   => (if New_Unit.Kind /= Subunit then
                                      New_Unit.Kind = Existing_Unit.Kind),
                       Message => "Library unit specification kind does not " 
                         &        "match the body - one is a package, " 
                         &        "and the other is a subprogram.");
               
               -- Append any inbound subunit bodies to the vector of the
               -- existing unit
               
               Existing_Unit.Subunit_Bodies.Append (New_Unit.Subunit_Bodies);
               
            when External_Unit =>
               
               -- The current unit had better be "requested"
               Assert (Existing_Unit.State = Requested,
                       Message => "Double registration of an "
                         &        "external unit");
               
         end case;
         
         -- Getting here means that the existing unit was not of Kind
         -- "Unknown", and so we really need to just add the appropriate
         -- source info.
         
         if New_Unit.Spec_File /= null then
            -- Spec
            Assert (Check => Existing_Unit.Spec_File = null,
                    Message => "Double registration of specification");
            Existing_Unit.Spec_File := New_Unit.Spec_File;
            
         end if;
         
         if New_Unit.Body_File /= null then
            -- Body
            Assert (Check   => Existing_Unit.Body_File = null,
                    Message => "Double registration of body");
            Existing_Unit.Body_File := New_Unit.Body_File;
            
         end if;
         
         Existing_Unit.State := Available;
         
         -- Note that we mark this as available even in cases where the unit
         -- might not technically be Available - for example, an Ada package
         -- with only a body. If we didn't set the state to Available here,
         -- the error to the user might end up being a lot more cryptic than
         -- if the failure happens during compilation.
         --
         -- Remember that AURA is not a compiler, and sometimes we need to
         -- step out of the way.
         
      end Modify_Unit;
      
      Insert_OK: Boolean;
      
   begin
      pragma Assert (New_Unit.State = Available);
      
      -- It is possible that multiple work orders (one for the body, one for
      -- the spec, and several for subunit bodies) for the same unit can be
      -- processed by multiple workers simultaneously. Only one of them can
      -- be the first to insert the new unit, and the rest have to modify.
      --
      -- We'll have a controled race to see who gets to insert by just trying
      -- to do that immediately via an Insert operation. If that operation
      -- fails, we'll just revert to a modification. This saves the hastly of
      -- checking first (via Contains_Element), and then running the risk that
      -- another worker will get in before us anyways, and cause the Insert
      -- operation to fail anyways.
      

      
      All_Library_Units.Insert (New_Item => New_Unit,
                                Inserted => Insert_OK);
      
      
      if not Insert_OK then
         All_Library_Units.Modify (Match   => New_Unit,
                                   Process => Modify_Unit'Access);
      end if;
      
      -- Now we need to check on the unit's Subsystem. If it is not already
      -- registered in All_Subsystems, we need to do one of two possible
      -- things:
      --
      -- 1. If the registration order is associated with an AURA subsystem,
      --    that subsystem better exist. That is because before an AURA
      --    subsystem is entered, it's Subsystem entry will be made first.
      --    This should not happen, and is a bug (pragma Assert is sufficient)
      --
      -- 2. If the registration is not associated with an AURA subsystem,
      --    it is part of the root project. We should then register a new
      --    subsystem based on the name of the unit. If a subsystem already
      --    exists, it should be both AURA and "Requested", since the root
      --    project items are allways entered before any of the AURA
      --    subsystems. Therefore we can use the Include operation, which
      --    will cause any existing subsystem requested (which are assumed to
      --    be AURA requests) to be replaced.
      --
      -- Note that we don't have a race condition issue like we did with the
      -- unit, since the installed non-aura Subsystem is always the same, and
      -- so we can just invoke Include. We do try checking first to save on
      -- the performance hit of actually overriting an existing element

      
      pragma Assert 
        (if Order.AURA then
            All_Subsystems.Contains_Element (Order.AURA_Subsystem));
      
      if not Order.AURA then
         declare
            New_SS: Subsystem (AURA => False);
            
         begin
            New_SS.Name  := Unit_Name(New_Unit.Name.Subsystem_Name);
            New_SS.State := Available;
            
            All_Subsystems.Include (New_SS);
            -- This of course overwrites anything that exists there. This
            -- only happens for non aura unit entires, where the other data
            -- besides name and state are not relevent.
            --
            -- The one exception would be the AURA package itself. Thus it
            -- is important that the the root configuration be processed after
            -- all of the AURA units have been entered.
            
         end;
      end if;
      
   end Execute;
   
end Registrar.Executive.Library_Unit_Registration;
