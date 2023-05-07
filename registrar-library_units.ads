------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019-2020, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- This package implements the Library_Unit type for us in tracking all 
-- "withed" units across the entire AURA project. This type is intended to act
-- as the Element_Type for the Hashed Set declared in the Units.Unit_Sets child
-- package

with Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded;

with Unit_Names;
with Unit_Names.Hash;
with Stream_Hashing;

with Registrar.Source_Files;

package Registrar.Library_Units is
   
   use type Unit_Names.Unit_Name;
   
   -- Library_Unit_State --
   ------------------------
   
   -- The Library_Unit_State pertains to each with'ed library unit in a 
   -- given Unit_Set. An Aquired Library_Unit_Status never regresses to
   -- Requested. If any Library_Unit cannot be Aquired, AURA fails.
   
   type Library_Unit_State is
     (Requested,
      -- Unit is needed but not yet Aquired
      --
      -- The Unit has been explicitly withed by any program unit of the AURA
      -- project, but that Unit was not present in the Set. It is added with
      -- the Requested State.
      
      Available,
      -- At least one source item for the unit has been entered, indicating
      -- that the source is available, but has not been compiled.
      
      Compiled);
      -- The library unit has been successfully compiled, or can be "assumed"
      -- compiled due to having no reverse dependencies.
      
   -- Library_Unit_Kind --
   -----------------------
   
   -- Library unit kind is used to control compilation, and specifically to
   -- control binding and linking of the main program/library. If the main
   -- unit is a package, the binder is instructed that there is "no main
   -- subprogram"
   
   type Library_Unit_Kind is
     (Unknown,
      -- For requested Ada units
      
      External_Unit,
      -- Unit is an external dependency
      
      Package_Unit,
      -- Library unit is a package
      
      Subprogram_Unit,
      -- Library unit is a subprogram
      
      Subunit);
      -- This kind is somewhat rare, and is not technically a "library unit",
      -- but occurs when subunits are nested. In order words, if a Library_Unit
      -- has this kind, it is itself a subunit of a library unit, which has
      -- it's own subunits.
      --
      -- Separate_Units are not directly compiled, just as the Subunit_Bodies
      -- vector
     
   
   ------------------
   -- Library_Unit --
   ------------------
   
   type Library_Unit is
      record
         Name      : Unit_Names.Unit_Name;
         State     : Library_Unit_State := Requested;
         Kind      : Library_Unit_Kind  := Unknown;
         
         Is_Generic: Boolean            := False;
         -- Is_Generic applies only to Package and Subprogram units, and
         -- specifically implies a generic library unit. Indeed, unit
         -- registration checks for illegal presentations, such as a "generic"
         -- body, or subunit
         
         Spec_File: Source_Files.Source_File_Access := null;
         Body_File: Source_Files.Source_File_Access := null;
         -- Source_Files are limited objects. The intention of any given
         -- Library_Unit is that each Source_File_Access value is initialized
         -- with an allocator exactly once, and then used for the life of the
         -- program (by being entered into the Registry)
         --
         -- Deallocation of these members should never be necessary.
         --
         -- There is some potential for memory leaks with this configuration,
         -- but that is not likely to be much of a problem given that the AURA 
         -- CLI is designed to run once to completion.
         --
         -- These members should only be allocated by Registrar.Enter_Unit, 
         -- which then passes the new unit to the registry.
         
         Subunit_Bodies: Source_Files.Source_File_Vectors.Vector;
         -- any subunit bodies are attached to the unit record so that the
         -- subsystem hashes may be generated
         
         -- Hashes --
         -- Note that these values are never meaninful when Kind is Subunit.
         -- Only library units (Package_Unit or Subprogram_Unit) retain hash
         -- values. The most immediate parent library unit of a tree of
         -- subunits contains a Body_Source_Hash that includes all subunits
         
         Specification_Hash : Stream_Hashing.Hash_Type;
         -- This hash is derrived from Spec_File.Hash, and is set during
         -- a Implementation_Hash run. Since Source_Files cannot be saved to
         -- Last_Run, this ensures that the Specification hashes can be saved
         -- without overly elaborate methods
         --
         -- This value is only valid if the unit has a spec
         
         Implementation_Hash: Stream_Hashing.Hash_Type;
         -- This is a collective hash that includes the Body_File,
         -- as well as all files in Subunit_Bodies, including children subunits
         -- of those subunits. This hash is valid after a Hash_Pass when Kind
         -- is not Subunit, and State is not Requested.
         --
         -- This value is only valid if the unit has a body
         
         Compilation_Hash: Stream_Hashing.Hash_Type;
         -- A collective hash of all compilation products of this unit.
         -- (For GNAT this would be the .o and .ali files).
         -- This value is not valid of Kind is "Subunit".
         --
         -- This value is valid only if State = Compiled
         
         
      end record;
   
   -- For implementation of Library_Units.Library_Unit_Sets
   function Library_Unit_Name_Hash (Unit: Library_Unit) 
                                   return Ada.Containers.Hash_Type
     is (Unit_Names.Hash (Unit.Name));
   
   function Equivalent_Units (Left, Right: Library_Unit) 
                             return Boolean
     is (Left.Name = Right.Name);
   
   -----------------------
   -- Library_Unit_Sets --
   -----------------------
   
   package Library_Unit_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Library_Unit,
      Hash                => Library_Unit_Name_Hash,
      Equivalent_Elements => Equivalent_Units);
   
   -- Key operations
   function Key (Unit: Library_Unit) return Unit_Names.Unit_Name is
     (Unit.Name);
   
   package Library_Unit_Sets_Keyed_Operations is
     new Library_Unit_Sets.Generic_Keys
       (Key_Type        => Unit_Names.Unit_Name,
        Key             => Key,
        Hash            => Unit_Names.Hash,
        Equivalent_Keys => Unit_Names."=");
   
end Registrar.Library_Units;
