------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019-2022, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- This package provides an interface for making read-only queries on the
-- registry. Naturally these operations are task-safe.

with Unit_Names, Unit_Names.Sets;
with Registrar.Subsystems;
with Registrar.Library_Units;

package Registrar.Queries is
   
   -----------------------
   -- Subsystem Queries --
   -----------------------
   
   function All_Subsystems return Subsystems.Subsystem_Sets.Set;
   -- Returns the full set of all library units in the registrar
   -- (Both entered and requested)
   
   function Requested_Subsystems return Subsystems.Subsystem_Sets.Set;
   
   -- Returns a set of all registered Subsystems where State = Requested,
   --
   -- An empty set, along with an empty set from Requested_Library_Units
   -- indicates that all dependencies have been satisfied.
   --
   -- Note. This query also implies AURA = True, since only AURA Subsystems can
   -- be in the "Requested" state.
   
   function Aquired_Subsystems return Subsystems.Subsystem_Sets.Set;
   
   -- Returns a set of all reigstered Subsystems where State = Aquired.
   --
   -- Note. This query also implies AURA = True since only AURA Subsystems can
   -- be in the "Aquired" state.
   
   function Available_Subsystems return Subsystems.Subsystem_Sets.Set;
   
   -- Returns a set of all registered Subsystems where State = Available
   --
   -- This query is informational only, and may be depreciated.
   
   function Unavailable_Subsystems return Subsystems.Subsystem_Sets.Set;
   
   -- Returns a set of all registered Subsystem where State = Unavailable
   -- (Checkout failed for "common" reasons). 
   
   function Subsystem_Registered (Name: Unit_Names.Unit_Name) return Boolean;
   -- True if a Subsystem by the name Name has been entered (state is not
   -- "Requested")
   
   function Lookup_Subsystem (Name: Unit_Names.Unit_Name) 
                             return Subsystems.Subsystem
     with Pre => Subsystem_Registered (Name);
   -- Returns a copy of the Subsystem record pertaining to unit Name.
   
   function Subsystem_Dependencies (Name: Unit_Names.Unit_Name)
                                   return Unit_Names.Sets.Set;
   -- Returns a set of names for all subsystems on which Name depends
   -- (Forward dependencies)
   
   function Dependent_Subsystems (Name: Unit_Names.Unit_Name)
                                 return Unit_Names.Sets.Set;
   
   -- Returns a set of names for all units that depend on the subsystem Name.
   -- (Reverse dependencies)
   
   --------------------------
   -- Library_Unit Queries --
   --------------------------
   
   use type Library_Units.Library_Unit_Kind;
   
   function All_Library_Units return Library_Units.Library_Unit_Sets.Set;
   
   -- Returns the full set of all library units in the registrar
   -- (Both entered and requested)
   
   
   function Subsystem_Library_Units (SS: Subsystems.Subsystem) 
                                    return Library_Units.Library_Unit_Sets.Set;
   
   -- Returns a set of all Library_Units associated with the given Subsytem
   
   
   function Requested_Library_Units return Library_Units.Library_Unit_Sets.Set;
   
   -- Returns a set of all registetred Subsystems where State = Requested
   --
   -- If Requested_AURA_Subunits returns an empty set, then any units returned
   -- by Requested_Library_Units indicates missing units
   
   
   function Entered_Library_Units return Library_Units.Library_Unit_Sets.Set;
   
   -- Returns a set of all registered Library Units that are not "Requested"
   -- (Available or Compiled)
   
   
   function Available_Library_Units return Library_Units.Library_Unit_Sets.Set;
   
   -- Returns a set of all registered Library Units where State = Available,
   -- excluding Ada Subunits (essentially all separately-compilable units)
   
   
   function Compiled_Library_Units return Library_Units.Library_Unit_Sets.Set;
   
   -- Returns a set of all registered Library Units where State = Compiled
   
   
   function Ada_Library_Units return Library_Units.Library_Unit_Sets.Set;
   
   -- Returns a set of all registered Ada library units (Package_Unit or
   -- Subprogram_Unit). Note that subunits are not actually library units
   -- according to Ada, and are not included.
   
   
   function External_Units return Library_Units.Library_Unit_Sets.Set;
   
   -- Returns a set of all registered non-Ada (external) units
   
   
   function Unit_Registered (Name: Unit_Names.Unit_Name) return Boolean;
   
   -- True if a Unit by the name Name has been registered at all
   -- (any state)
   
   
   function Unit_Entered (Name: Unit_Names.Unit_Name) return Boolean;
   
   -- True if a Unit by the name Name has been entered (state is not
   -- "Requested")
   
   
   function Lookup_Unit (Name: Unit_Names.Unit_Name)
                        return Library_Units.Library_Unit
   with Pre => Unit_Registered (Name);
   
   -- Returns a copy of the Library_Unit record pertaining to unit Name.
   
   
   Orphaned_Subunit: exception;
   
   function Trace_Subunit_Parent (Unit: Library_Units.Library_Unit)
                                 return Library_Units.Library_Unit
   with Pre => Unit_Entered (Unit.Name) and then
               Lookup_Unit (Unit.Name).Kind = Library_Units.Subunit,
     Post => Trace_Subunit_Parent'Result.Kind 
             in Library_Units.Package_Unit | Library_Units.Subprogram_Unit;
   
   -- Returns the Parent Library Unit of a given Subunit. Orphaned subunits
   -- are explicitly detected, and Orphaned_Subunit is raised with a message
   -- identifying the full name of the subunit.
   
   
   function Unit_Dependencies (Name: Unit_Names.Unit_Name)
                              return Unit_Names.Sets.Set
   with Pre => Unit_Registered (Name);
   
   function Unit_Dependencies (Unit: Library_Units.Library_Unit) 
                              return Library_Units.Library_Unit_Sets.Set
   with Pre => Unit_Registered (Unit.Name);
   
   -- Returns a set of names/units for all units on which Name/Unit depends
   -- (Forward dependencies)
   
   
   function Dependent_Units (Name: Unit_Names.Unit_Name)
                            return Unit_Names.Sets.Set
   with Pre => Unit_Registered (Name);
   
   function Dependent_Units (Unit: Library_Units.Library_Unit) 
                            return Library_Units.Library_Unit_Sets.Set
   with Pre => Unit_Registered (Unit.Name);
   
   -- Returns a set of names/units for all units that depend on Unit/Name. 
   -- (Reverse dependencies)
                            
   
end Registrar.Queries;
