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

with Ada.Containers.Hashed_Sets; 

with Registrar.Registry;
with Registrar.Protected_Map;

package body Registrar.Queries is
   
   --------------------
   -- All_Subsystems --
   --------------------
   
   function All_Subsystems return Subsystems.Subsystem_Sets.Set is
     (Registry.All_Subsystems.Extract_Set);
   
   --------------------------
   -- Requested_Subsystems --
   --------------------------
   
   function Requested_Subsystems return Subsystems.Subsystem_Sets.Set is
      use Subsystems;
      
      pragma Assertion_Policy (Post => Check);
      
      function Is_Requested (S: Subsystem) return Boolean
        is (S.State = Requested)
      with Post => (if Is_Requested'Result then S.AURA);
      
   begin
      return Registry.All_Subsystems.Extract_Subset (Is_Requested'Access);
   end Requested_Subsystems;
   
   ------------------------
   -- Aquired_Subsystems --
   ------------------------
   
   function Aquired_Subsystems return Subsystems.Subsystem_Sets.Set is
      use Subsystems;
      
      pragma Assertion_Policy (Post => Check);
      
      function Is_Aquired (S: Subsystem) return Boolean
        is (S.State = Aquired)
      with Post => (if Is_Aquired'Result then S.AURA);
      
   begin
      return Registry.All_Subsystems.Extract_Subset (Is_Aquired'Access);
   end Aquired_Subsystems;
   
   --------------------------
   -- Available_Subsystems --
   --------------------------
   
   function Available_Subsystems return Subsystems.Subsystem_Sets.Set is
      use Subsystems;
      
      function Is_Available (S: Subsystem) return Boolean
        is (S.State = Available);
   begin
      return Registry.All_Subsystems.Extract_Subset (Is_Available'Access);
   end Available_Subsystems;
   
   ----------------------------
   -- Unavailable_Subsystems --
   ----------------------------
   
   function Unavailable_Subsystems return Subsystems.Subsystem_Sets.Set is
      use Subsystems;
      
      function Is_Unavailable (S: Subsystem) return Boolean
        is (S.State = Unavailable);
   begin
      return Registry.All_Subsystems.Extract_Subset (Is_Unavailable'Access);
   end Unavailable_Subsystems;
   
   --------------------------
   -- Subsystem_Registered --
   --------------------------
   
   function Subsystem_Registered (Name: Unit_Names.Unit_Name) return Boolean
   is 
      use Subsystems;
      
      Match_SS: constant Subsystem
        := (Name   => Name,
            others => <>);
   begin
      if not Registry.All_Subsystems.Contains_Element (Match_SS) then
         return False;
         
      else
         return Registry.All_Subsystems.Extract_Element (Match_SS).State 
           /= Requested;
         
      end if;
   end Subsystem_Registered;
   
   ----------------------
   -- Lookup_Subsystem --
   ----------------------
   
   function Lookup_Subsystem (Name: Unit_Names.Unit_Name) 
                             return Subsystems.Subsystem
   is
      use Subsystems;
      
      Match_SS: constant Subsystem := (Name   => Name,
                                       others => <>);
   begin
      return Registry.All_Subsystems.Extract_Element (Match_SS);
   end Lookup_Subsystem;
   
   ----------------------------
   -- Subsystem_Dependencies --
   ----------------------------
   
   function Subsystem_Dependencies (Name: Unit_Names.Unit_Name)
                                   return Unit_Names.Sets.Set
   is
      use Unit_Names;
      use Subsystems;
      use Registrar.Registry;
      
      Dependency_Names: Unit_Names.Sets.Set;
      Match_Set: Subsystem_Sets.Set;
      
   begin
      if not Registry.Subsystem_Forward_Dependencies.Contains_Element (Name) 
      then
         return Unit_Names.Sets.Empty_Set;
      else
         return Registry.Subsystem_Forward_Dependencies.Extract_Element (Name);
      end if;
   end Subsystem_Dependencies;
   
   --------------------------
   -- Dependent_Subsystems --
   --------------------------
   
   function Dependent_Subsystems (Name: Unit_Names.Unit_Name)
                                 return Unit_Names.Sets.Set
   is begin
      if not Registry.Subsystem_Reverse_Dependencies.Contains_Element (Name)
      then
         return Unit_Names.Sets.Empty_Set;
      else
         return Registry.Subsystem_Reverse_Dependencies.Extract_Element (Name);
      end if;
   end Dependent_Subsystems;
   
   -----------------------
   -- All_Library_Units --
   -----------------------
   
   function All_Library_Units return Library_Units.Library_Unit_Sets.Set is
     (Registry.All_Library_Units.Extract_Set);
   
   -----------------------------
   -- Subsystem_Library_Units --
   -----------------------------
   
   function Subsystem_Library_Units (SS: Subsystems.Subsystem) 
                                    return Library_Units.Library_Unit_Sets.Set
   is 
      use Unit_Names;
      use Subsystems;
      use Library_Units;

      function In_Subsystem (Unit: Library_Unit) return Boolean is
        (Unit_Name (Unit.Name.Subsystem_Name) = SS.Name);
      
   begin
      return Registry.All_Library_Units.Extract_Subset (In_Subsystem'Access);
   end Subsystem_Library_Units;
   
   -----------------------------
   -- Requested_Library_Units --
   -----------------------------
   
   function Requested_Library_Units 
     return Library_Units.Library_Unit_Sets.Set 
   is
      use Library_Units;
      
      function Is_Requested (U: Library_Unit) return Boolean
        is (U.State = Requested);
   begin
      return Registry.All_Library_Units.Extract_Subset (Is_Requested'Access);
   end Requested_Library_Units;
   
   ---------------------------
   -- Entered_Library_Units --
   ---------------------------
   
   function Entered_Library_Units 
     return Library_Units.Library_Unit_Sets.Set 
   is
      use Library_Units;
      
      function Is_Entered (U: Library_Unit) return Boolean
        is (U.State /= Requested);
   begin
      return Registry.All_Library_Units.Extract_Subset (Is_Entered'Access);
   end Entered_Library_Units;
   
   -----------------------------
   -- Available_Library_Units --
   -----------------------------
   
   function Available_Library_Units
     return Library_Units.Library_Unit_Sets.Set
   is
      use Library_Units;
      
      function Is_Available (U: Library_Unit) return Boolean is 
        (U.State = Available 
            and then U.Kind in External_Unit | Package_Unit | Subprogram_Unit);
   begin
      return Registry.All_Library_Units.Extract_Subset (Is_Available'Access);
   end Available_Library_Units;
   
   ----------------------------
   -- Compiled_Library_Units --
   ----------------------------
   
   function Compiled_Library_Units
     return Library_Units.Library_Unit_Sets.Set
   is
      use Library_Units;
      
      function Is_Compiled (U: Library_Unit) return Boolean
        is (U.State = Compiled);
   begin
      return Registry.All_Library_Units.Extract_Subset (Is_Compiled'Access);
   end Compiled_Library_Units;
   
   -----------------------
   -- Ada_Library_Units --
   -----------------------
   
   function Ada_Library_Units return Library_Units.Library_Unit_Sets.Set is
      use Library_Units;
      
      function Is_Ada_Unit (U: Library_Unit) return Boolean is
        (U.Kind in Package_Unit | Subprogram_Unit);
   begin
      return Registry.All_Library_Units.Extract_Subset (Is_Ada_Unit'Access);
   end Ada_Library_Units;
   
   --------------------
   -- External_Units --
   --------------------
   
   function External_Units return Library_Units.Library_Unit_Sets.Set is
      use Library_Units;
      
      function Is_External_Unit (U: Library_Unit) return Boolean is
        (U.Kind = External_Unit);
   begin
      return Registry.All_Library_Units.Extract_Subset 
        (Is_External_Unit'Access);
   end External_Units;
   
   ---------------------
   -- Unit_Registered --
   ---------------------
   
   function Unit_Registered (Name: Unit_Names.Unit_Name) return Boolean is
      use Library_Units;
      
      Match_Unit: constant Library_Unit
        := (Name   => Name,
            others => <>);
   begin
      return Registry.All_Library_Units.Contains_Element (Match_Unit);
   end Unit_Registered;
   
   ------------------
   -- Unit_Entered --
   ------------------
   
   function Unit_Entered (Name: Unit_Names.Unit_Name) return Boolean is
      use Library_Units;
      
      Match_Unit: constant Library_Unit
        := (Name   => Name,
            others => <>);
   begin
      if not Registry.All_Library_Units.Contains_Element (Match_Unit) then
         return False;
         
      else
         return Registry.All_Library_Units.Extract_Element (Match_Unit).State 
           /= Requested;
         
      end if;
   end Unit_Entered;
   
   -----------------
   -- Lookup_Unit --
   -----------------
   
   function Lookup_Unit (Name: Unit_Names.Unit_Name)
                        return Library_Units.Library_Unit
   is
      use Library_Units;
      
      Match_Unit: constant Library_Unit
        := (Name   => Name,
            others => <>);
   begin
      return Registry.All_Library_Units.Extract_Element (Match_Unit);
   end Lookup_Unit;
   
   --------------------------
   -- Trace_Subunit_Parent --
   --------------------------
   
   function Trace_Subunit_Parent (Unit: Library_Units.Library_Unit)
                                 return Library_Units.Library_Unit
   is
      use Library_Units;
   begin
      return Tracer: Library_Unit := Lookup_Unit (Unit.Name.Parent_Name) do
         while Tracer.Kind = Subunit loop
            Tracer := Lookup_Unit (Tracer.Name.Parent_Name);
         end loop;
      end return;
   end Trace_Subunit_Parent;
   
   -----------------------
   -- Unit_Dependencies --
   -----------------------
   
   function Unit_Dependencies (Name: Unit_Names.Unit_Name)
                              return Unit_Names.Sets.Set
   is begin
      if not Registry.Unit_Forward_Dependencies.Contains_Element (Name) then
         return Unit_Names.Sets.Empty_Set;
      else
         return Registry.Unit_Forward_Dependencies.Extract_Element (Name);
      end if;
   end Unit_Dependencies;
   
   ----------------------------------------------------------------------
   
   function Unit_Dependencies (Unit: Library_Units.Library_Unit) 
                              return Library_Units.Library_Unit_Sets.Set
   is 
      use Library_Units;
      
      Subject: constant Library_Unit := Lookup_Unit (Unit.Name);
      Dependency_Names: constant Unit_Names.Sets.Set
        := Unit_Dependencies (Subject.Name);
      Dependency_Lookup: Library_Unit_Sets.Set;
   begin
      for Name of Dependency_Names loop
         Dependency_Lookup.Insert (Library_Unit'(Name => Name, others => <>));
      end loop;
      
      return Registry.All_Library_Units.Extract_Subset (Dependency_Lookup);
   end Unit_Dependencies;
   
   ---------------------
   -- Dependent_Units --
   ---------------------
   
   function Dependent_Units (Name: Unit_Names.Unit_Name)
                            return Unit_Names.Sets.Set
   is begin
      if not Registry.Unit_Reverse_Dependencies.Contains_Element (Name) then
         return Unit_Names.Sets.Empty_Set;
      else
         return Registry.Unit_Reverse_Dependencies.Extract_Element (Name);
      end if;
   end Dependent_Units;
   
   ----------------------------------------------------------------------
   
   function Dependent_Units (Unit: Library_Units.Library_Unit) 
                            return Library_Units.Library_Unit_Sets.Set
   is 
      use Library_Units;
      
      Subject: constant Library_Unit := Lookup_Unit (Unit.Name);
      Dependent_Names: constant Unit_Names.Sets.Set
        := Dependent_Units (Subject.Name);
      Dependents_Lookup: Library_Unit_Sets.Set;
   begin
      for Name of Dependent_Names loop
         Dependents_Lookup.Insert (Library_Unit'(Name => Name, others => <>));
      end loop;
      
      return Registry.All_Library_Units.Extract_Subset (Dependents_Lookup);
   end Dependent_Units;
   
end Registrar.Queries;
