------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2024, ANNEXI-STRAYLINE Inc.                          --
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
with Ada.Directories;
with Ada.Characters.Conversions;
with Ada.Streams.Stream_IO;

with Ada_Lexical_Parser;
with Unit_Names.Sets;
with Registrar.Registry;
with Registrar.Subsystems;
with Registrar.Library_Units;
with Registrar.Registration;
with Registrar.Source_Files.Allocation;
with Registrar.Executive.Library_Units_Request;
with Registrar.Executive.Library_Unit_Registration;

separate (Registrar.Executive.Unit_Entry)
   
procedure Execute (Order: in out Unit_Entry_Order) is
   use Registrar.Library_Units;
   use Registrar.Executive.Library_Units_Request;
   use Registrar.Executive.Library_Unit_Registration;
   use Registrar.Source_Files;
   
   use type Ada.Containers.Count_Type;
   
   function To_Wide_Wide_String (Item: in String) return Wide_Wide_String
     renames Ada.Characters.Conversions.To_Wide_Wide_String;
   
   New_Unit_Registration: Library_Unit_Registration_Order (AURA => Order.AURA);
   New_Unit: Library_Unit renames New_Unit_Registration.Registrant;
   
   Dependency_Request: Library_Units_Request_Order;
   Dependencies      : Library_Unit_Sets.Set 
     renames Dependency_Request.Requested_Units;
   
   use type Unit_Names.Unit_Name;
   
   procedure Assert (Check: in Boolean; Message: in String)
     renames Ada.Assertions.Assert;
   
   package Source_Pack is
      use Registrar.Source_Files;
      
      type Source_File_Type is (Ada_Spec, Ada_Body, Non_Ada);
      subtype Ada_File_Type is Source_File_Type range Ada_Spec .. Ada_Body;
      
      subtype Ada_File_Extension is String (1 .. 4);
      
      Ada_File_Extensions: constant array (Ada_File_Type) of Ada_File_Extension
        := (Ada_Spec => ".ads", Ada_Body => ".adb");
   
      Unit_Source_Type: Source_File_Type;
      
      Source: Source_File_Access := Registrar.Source_Files.Allocation.Open
        (UBS.To_String (Order.File_Full_Name));
      
      Stream: aliased Source_Stream := Checkout_Read_Stream (Source);
      
      procedure Discard_Source;
      -- Closes Source and then Destroys it.
      -- This nullfies Source and makes the Stream unusable
      
      Can_Discard: Boolean := True;
      -- Indicates that it is safe to call Discard_Source (which deallocates
      -- the Source_File object pointed to by Source). If it is possible that
      -- the New_Unit_Registration work order has been submitted,
      -- Discard_Source becomes unsafe
   end Source_Pack;
   
   
   package Parse_Pack is
      use Ada_Lexical_Parser;
      
      function Content  return Wide_Wide_String;
      function Category return Lexical_Category;
      -- Of the more recent element parsed
      
      procedure Next_Element;
      -- Parse the next non-comment element of the Source
      
      procedure Skip_To_Semicolon;
      -- Skip all elements until reaching a semi-colon
      
      function  File_And_Position return String;
      -- Return a string in the format "path/to/file.ads: 12: 1"
      
      procedure Abort_Parse;
      -- Raise an Invalid_Unit exception with information identifiying
      -- the location of the offending element in the source
      
   private
      -- TODO: Move these back int othe package body at some point. They were
      -- moved here due to a (temporary) bug in GCC 14.2, once 14.2 becomes
      -- old enough, this can be reverted.
      
      Source: Source_Buffer (Source_Pack.Stream'Access);
      -- The parse buffer
   
      E: Lexical_Element;
      -- Last element parsed
      
   end Parse_Pack;
   
   
   package Process_Pack is
      
      procedure Process_With;
      -- All library units including implicitly with'ed parent units of a with
      -- statement are included in the Dependencies set.
      
      procedure Process_External_With;
      -- All external unit file names of an External_With pragma are staged
      -- in lieu of a verified subsystem name, to be included in the
      -- Dependencies set during the Process_Declaration phase.
      
      procedure Process_Declaration;
      -- Identifies the unit name, or the parent unit name of a subunit,
      -- and sets that as the Name of New_Unit
      --
      -- This also sets the Kind property of the New_Unit
      
      procedure Filter_Standard_Library;
      -- Removes from the Dependencies set all units that are part of the
      -- standard Ada library (particularily Ada, Interfaces, and GNAT)
      
      procedure Process_Parent_Dependencies;
      -- Adds all implicit withs of parent units to the Dependencies set,
      
      procedure Process_Staged_Externals;
      -- Prepends New_Unit.Name.Subsystem, plus a '%' to the name of each unit
      -- of the set of staged external with units, and then unions that set
      -- with Dependencies
      
   private
      -- TODO: See private part of Parse_Pack above.
      
      Staged_Externals: Library_Unit_Sets.Set;
      -- All External_With units that have not yet had their subsystem name
      -- prepended, or included in the Dependencies set
      
   end Process_Pack;
   
   package body Source_Pack  is separate;
   package body Parse_Pack   is separate;
   package body Process_Pack is separate;
   
begin
   -- Attach trackers
   New_Unit_Registration.Tracker := Registration.Entry_Progress'Access;
   Dependency_Request.Tracker    := Registration.Entry_Progress'Access;
   
   pragma Assert (if Order.AURA then Order.AURA_Subsystem.AURA);
   
   -- First try to classify the source type by file extension. If it does
   -- not appear to be an Ada source, we don't even need to open the file,
   -- or do any other special processing
   
   declare
      use UBS;
      use Source_Pack;
      
      File_Name: Unbounded_String renames Order.File_Full_Name;
      Last_Dot_Index: Natural := Index (Source  => File_Name,
                                        Pattern => ".",
                                        Going   => Ada.Strings.Backward);
      Extension: Unbounded_String;
   begin
      pragma Assert (Length (File_Name) > 0);
      
      if Last_Dot_Index > 0 then
         Extension := Unbounded_Slice (Source => File_Name,
                                       Low    => Last_Dot_Index,
                                       High   => Length (File_Name));
      end if;
      
      -- Propegate the reported subsystem
      if New_Unit_Registration.AURA then
         New_Unit_Registration.AURA_Subsystem := Order.AURA_Subsystem;
      end if;
      
      if Extension = Ada_File_Extensions (Ada_Spec) then
         Unit_Source_Type := Ada_Spec;
      elsif Extension = Ada_File_Extensions (Ada_Body) then
         Unit_Source_Type := Ada_Body;
      else
         -- Not an Ada source. Generate external dependency name and enter
         -- the unit, to allow it to be refrenced from Ada sources through
         -- the AURA-specific pragma External_With
         
         -- The source file itself takes place of the body, for later
         -- hashing
         
         -- If the file itself is part of the root project, we make it a
         -- conceptual child of standard. Similarily, if we see an external
         -- with in a non-AURA unit.
         
         New_Unit.Name.Set_Name
           ((if Order.AURA then 
                Order.AURA_Subsystem.Name.To_String
             else
                "standard") 
              & '%' 
              & To_Wide_Wide_String 
                (Ada.Directories.Simple_Name(To_String (File_Name))));
         
         -- Submit the registration order
         
         New_Unit.State     := Available;
         New_Unit.Kind      := External_Unit;
         New_Unit.Body_File := Source;
         New_Unit_Registration.Tracker.Increment_Total_Items;
         Workers.Enqueue_Order (New_Unit_Registration);
         
         -- We also need to register an empty forward dependency set. We can do
         -- a regular Insert which should always work because external units
         -- cannot have subunits
         
         declare
            Inserted: Boolean;
         begin
            Registry.Unit_Forward_Dependencies.Insert
              (Key      => New_Unit.Name,
               New_Item => Unit_Names.Sets.Empty_Set,
               Inserted => Inserted);
            
            pragma Assert (Inserted);
         end;
         return;
         
      end if;
   end;
   
   -- The New_Unit_Registration order is now filled-out except for the Name,
   -- which will be parsed from the Ada source itself
   
   declare
      use Ada_Lexical_Parser;
      use Parse_Pack;
      use Process_Pack;
      
   begin
      -- Note that we should not ever reach the end of file before we've
      -- finished our job here, so any exception that occurs due to reaching
      -- the end of the file causes failure of the entry order as expected
      
      -- Our job is to collect the dependencies (with and pragma
      -- External_With), and then verify and register then name of the
      -- unit itself. We check for syntax errors were possible, and if
      -- anything is a miss, we abort the parse.
      
      Next_Element;
      
      loop
         
         -- Everything we are processing starts with a reserved word
         if Category /= Reserved_Word then
            Abort_Parse;
         end if;
         
         if Content = "with" then 
            Process_With;
            Next_Element;
            
         elsif Content = "use" then
            Skip_To_Semicolon;
            Next_Element;
            
         elsif Content = "pragma" then 
            Next_Element; 
            
            if Category = Identifier then
               if Content = "external_with" then
                  Process_External_With;
               else
                  -- Otherwise we ignore it
                  Skip_To_Semicolon;   
               end if;
               
               Next_Element;
            else
               Abort_Parse;
            end if;
            
         elsif Content = "private" then
            -- We expect either package or a with statement to follow
            Next_Element;
            
            if Category /= Reserved_Word then
               Abort_Parse;
            end if;
            
            if Content = "with" then
               Process_With;
               Next_Element;
               
            elsif Content = "package" then
               Process_Declaration;
               exit;
            end if;
            
         elsif Content = "limited" then
            -- We expect only a with to follow
            Next_Element;
            
            if Category = Reserved_Word 
              and then Content = "with"
            then
               Process_With;
               Next_Element;
            else
               Abort_Parse;
            end if;
            
         elsif Content = "generic" then
            -- We need to skip the generic formal part of the generic
            -- declaration. This might seem overly simplistic, but it should be
            -- correct if you think about it.
            --
            -- Within a generic declaration's generic formal part, the only
            -- time we will see something following a semicolon that is not a
            -- reserved word is if it is a parameter specification of the 
            -- formal part of a parameter profile, and in all cases we will
            -- never see a "generic formal parameter declaration" that does not
            -- begin- with "use" or "with", therefore we can just see if we 
            -- hit the subprogram or package specification (reserved word
            -- package, function, or procedure), or else we skip to the next
            -- semicolon until we do.
            --
            -- This is a great example of how well the Ada syntax is actually
            -- designed.
            
            loop
               Next_Element;
               exit when Category = Reserved_Word
                 and then Content in "package" | "function" | "procedure";
               Skip_To_Semicolon;
            end loop;
            
            New_Unit.Is_Generic := True;
            Process_Declaration;
            exit;
            
            -- Process_Declaration will ensure this is actually a spec not
            -- a body.
            
         elsif Content in 
           "separate" | "package" | "function" | "procedure" 
         then
            Process_Declaration;
            -- This completes the parsing pass
            exit;

         else
            -- This is unexpected
            Abort_Parse;
         end if;
         
      end loop;
      
      -- With the declaration processed, we filter out standard library with's
      -- and process the staged externals
      Filter_Standard_Library;
      Process_Parent_Dependencies;
      Process_Staged_Externals;
   end;
   
   -- We now have collected all required dependencies. We can now submit the
   -- Dependency_Request (if any), and also update the
   -- Unit_Forward_Dependencies and Unit_Reverse_Dependencies maps
   
   if not Dependencies.Is_Empty then
      -- Get these sent off now since there will be no more changes to the
      -- Dependencies set
      Dependency_Request.Tracker.Increment_Total_Items;
      Workers.Enqueue_Order (Dependency_Request);
   end if;
   
   -- Now lets also register the forward and unit dependencies
   --
   -- Interestingly enough, this will correctly associate dependencies of
   -- immediate subunits to the parent unit, since New_Unit's name is set
   -- according to the "parent" unit in the case of subunits. In other words,
   -- if this unit is a subunit, the dependent units will be told that the
   -- parent of this unit is depending on it - which is correct.
   --
   -- A later pass of Registrar.Dependency_Processing.Consolidate_Dependencies
   -- will merge all subunit forward dependency maps to their library-unit
   -- parent, and then build a reverse dependency map from the forward map
   --
   -- Every entered unit must have an entry, even if that entry is an empty
   -- set.
   --
   -- Note that we do not add our subystem to the subsystem forward dependency
   -- set here since that information is implicit in the unit dependencies.
   -- Also it is more efficient to build that set during the
   -- Consolidate_Dependencies processes.
   
   declare
      Forward_Dependencies: Unit_Names.Sets.Set;
      
      procedure Include_Forward_Dependencies (Set: in out Unit_Names.Sets.Set)
      is begin
         Set.Union (Forward_Dependencies);
      end Include_Forward_Dependencies;
      
   begin
      for Unit of Dependencies loop
         Forward_Dependencies.Insert (Unit.Name);
      end loop;
      
      Registry.Unit_Forward_Dependencies.Insert_Or_Modify
        (Key              => New_Unit.Name,
         New_Item         => Forward_Dependencies,
         Process_Existing => Include_Forward_Dependencies'Access);
      
   end;
   
   
   -- AURA Subsystems (i.e. units of a subsystem, i.e. units not in the project
   -- root) need special treatment, as does the actual AURA Subsystem itself!
   
   if New_Unit.Name.Subsystem_Name.To_String = "aura" then
      
      -- AURA subsystems themselves should not have any units of the AURA
      -- subsystem. This can often happen by accident when preparing a manifest
      -- so we will both do a hard assertion, and give some kind of meaninful
      -- message if this happens
      
      Assert (Check => not Order.AURA,
              Message => "Check manifest: AURA subsystems should not have " 
                & "units of the AURA subsystem itself.");
      
      -- The AURA subsystem strictly only contains packages, but we allow
      -- first order Subunits generally. Due to in processing those we won't
      -- know yet if a Subunit is part of a Package, but this will eventually
      -- be caught below.
      
      Assert (Check => New_Unit.Kind in Package_Unit | Subunit,
              Message => "The AURA subsystem shall only contain packages "
                & "and/or subunits.");
      
      -- And they shall not be generic
      
      Assert (Check => not New_Unit.Is_Generic,
              Message => "The AURA subsystem shall not contain generic "
                & "(library) units.");
      
      -- Units of the AURA subsystem shouldn't ever have dependencies (besides
      -- the standard library) that are not also AURA subsystem units. 
      --
      -- Also, any grandchildren of AURA shall be named "checkout". Recall that
      -- AURA children are either Repository units ("repository_x") - which
      -- cannot have children, or configuration units with the name of their
      -- respective subsystem - who shall only have a checkout child.
      --
      -- The problem is when a user modifies a Configuration unit. We need
      -- to at least provide some feedback if they try to get too fancy by
      -- either with'ing non-standard library units, or trying to add a child
      -- unit
      
      Assert
        (Check   => New_Unit.Subunit_Bodies.Length = 0,
         Message => "Units of the AURA subsystem may not have subunits.");
      
      for Unit of Dependencies loop
         Assert (Check => Unit.Name.Subsystem_Name.To_String = "aura",
                 Message => "Units of the AURA subsystem shall not have"
                   &        "outside dependencies besides the standard " 
                   &        "library.");
      end loop;
      
      
      if New_Unit.Name.To_String /= "aura"
        and then New_Unit.Name.Parent_Name.To_String /= "aura"
      then
         -- This is a (great-xx) grandchild of the AURA unit
         Assert 
           (Check => New_Unit.Name.Parent_Name.Parent_Name.To_String = "aura",
            Message => "The AURA subsystem heirarchy cannot be deeper "
              &        "than 3 levels");
         
         -- This is a grandchild of the AURA unit
         Assert 
           (Check => New_Unit.Name.Self_Direct_Name.To_String = "checkout",
            Message => "AURA subsystem child packages cannot have "
              &        "children (or nested subunits) except for "
              &        "checkout package specs.");
      end if;
      

   elsif Order.AURA then
      -- The provided subsystem and the unit's subsystem must match.
      -- Each AURA Subsystem shall only contain that subsystem, and
      -- the manifest (Subsystem.AURA) only
      
      Assert
        (Check => 
           Order.AURA_Subsystem.Name 
             = Unit_Names.Unit_Name(New_Unit.Name.Subsystem_Name),
         Message => 
           "Subsystem of entered unit does not match that " &
             "of the AURA subsystem to which is should belong");
      
      -- Police configuration manifests and configuration units.
      -- Configuration packages (spec and body) may not have any AURA
      -- dependencies, or dependencies on their own subsystem. Ergo they can
      -- only depend on the standard library, or possibly the compiler library
      -- (e.g. GNAT). 
      --
      -- Manifests also may not have any children
      --
      -- Note that the ultimate configuration unit is the manifest renamed
      -- (in the physical sense, not the Ada sense) to be AURA.Subsystem, and
      -- so implicitly with's the root AURA package. This also means that
      -- any unit that is part of the AURA subsystem face the same
      -- restrictions - which is checked above
      
      
      declare
         use Unit_Names;
         Subsys_Name   : constant Unit_Name 
           := Unit_Name (New_Unit.Name.Subsystem_Name);
         
         Check_Manifest: constant Unit_Name := Subsys_Name & ".AURA";
         Check_Children: constant Unit_Name := Check_Manifest & ".";
         
         Exclude_Parent: constant Library_Unit := (Name   => Subsys_Name,
                                                   others => <>);
      begin
         if New_Unit.Name = Check_Manifest then
            -- A properly formed manfiest will have one dependency, which
            -- will be the implicit dependency on subsystem's root unit
            
            Dependencies.Exclude (Exclude_Parent);
            Assert
              (Check => Dependencies.Is_Empty,
               Message => "AURA Configuration Manifests must only depend on "
                 &        "the Standard Library.");
            
            Assert
              (Check   => New_Unit.Kind = Package_Unit,
               Message => "AURA Configuration Manifest must be a package.");
            Assert
              (Check   => New_Unit.Subunit_Bodies.Length = 0,
               Message => "AURA Configuration Manifest may not have " 
                 &        "subunits.");
         else            
            Ada.Assertions.Assert
              (Check   => 
                 not New_Unit.Name.Match_Initial (Check_Children.To_String),
               Message =>
                 "AURA Configuration Manifests may not have children or "
                   & "subunits.");
         end if;
      end;
      
   end if;
   
   
   -- Assign the Source file to the appropriate location
   declare
      use Source_Pack;
   begin
      if New_Unit.Kind = Subunit then
         -- Even though subunits are not actually library units, we still
         -- need to keep the file handle around for hashing, and so we still
         -- submit the registration. For first order subunits, these will
         -- properly be attached to the parent library unit.
         --
         -- For second-order subunits (subunits of subunits), the dependency
         -- on the parent is entered as appropriate, so changes in these
         -- subunits will still trigger recompilation of the associated
         -- library unit. See also the discussion in the reverse dependency
         -- mapping part above
         --
         -- As a consequence, for second-order subunits, the "parent" subunit
         -- will have a "library unit" registered but will have no spec or body
         -- file. It's perhapse not great for it to be considered a "library
         -- unit", but the mechanics work-out.
         --
         -- The compiler will ultimately reject any funny buisness where,
         -- for example, there is an actual child package with the same name
         -- as a subunit - which isn't allowed anyways. (RM 10.1.3-14)
         
         Assert (Check   => Unit_Source_Type = Ada_Body,
                 Message => "Subunits shall always be bodies.");
         
         New_Unit.Subunit_Bodies.Append (Source);
         
      else
         case Unit_Source_Type is
            when Ada_Spec =>
               New_Unit.Spec_File := Source;
               
            when Ada_Body =>
               New_Unit.Body_File := Source;
               
            when Non_Ada =>
               -- This won't be possible if the implementation is correct.
               raise Program_Error;
         end case;
      end if;
   end;
   
   Source_Pack.Can_Discard := False;
   
   -- Finally, submit the new registration
   New_Unit.State := Available;
   New_Unit_Registration.Tracker.Increment_Total_Items;
   Workers.Enqueue_Order (New_Unit_Registration);
      
exception
   when others =>
      if Source_Pack.Can_Discard then
         Source_Pack.Discard_Source;
      end if;
      raise;
end Execute;
