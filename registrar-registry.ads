------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
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

-- This package contains the All_Library_Units and All_Subsystems sets and
-- access synchronization

with Unit_Names, Unit_Names.Sets;

with Registrar.Protected_Set;
with Registrar.Protected_Map;

with Registrar.Library_Units;
with Registrar.Subsystems;
with Registrar.Dependency_Maps;

private package Registrar.Registry is
   
   -----------------------
   -- All_Library_Units --
   -----------------------
   
   package All_Library_Units is new Protected_Set
     (Element_Type => Registrar.Library_Units.Library_Unit,
      Sets         => Registrar.Library_Units.Library_Unit_Sets);
   
   -------------------------------
   -- Unit_Forward_Dependencies --
   -------------------------------
   
   -- A given Library_Unit name ket maps to a set of all Library_Unit
   -- names that the given Library_Unit depends on. This includes any
   -- dependencies of the unit's subunits (if any)
   
   package Unit_Forward_Dependencies is new Protected_Map
     (Key_Type     => Unit_Names.Unit_Name,
      Element_Type => Unit_Names.Sets.Set,
      Maps         => Dependency_Maps);
   
   -------------------------------
   -- Unit_Reverse_Dependencies --
   -------------------------------
   
   -- A given Library_Unit name key maps to a set of all Library_Unit
   -- names that depend on the given Library_Unit. This is used to trace
   -- recompilation dependencies
   
   package Unit_Reverse_Dependencies is new Protected_Map
     (Key_Type     => Unit_Names.Unit_Name,
      Element_Type => Unit_Names.Sets.Set,
      Maps         => Dependency_Maps);
   
   --------------------
   -- All_Subsystems --
   --------------------
   
   package All_Subsystems is new Protected_Set
     (Element_Type => Registrar.Subsystems.Subsystem,
      Sets         => Registrar.Subsystems.Subsystem_Sets);

   ------------------------------------
   -- Subsystem_Forward_Dependencies --
   ------------------------------------
   
   -- A given Subsystem name key maps to a set of all Subsystem names
   -- on which the given Subsystem depends. This is used to ensure that
   -- all Subsystems checked-out from a "System" repository only depend
   -- on other subsystems that are also from the same repository
   
   package Subsystem_Forward_Dependencies is new Protected_Map
     (Key_Type     => Unit_Names.Unit_Name,
      Element_Type => Unit_Names.Sets.Set,
      Maps         => Dependency_Maps);
   
   ------------------------------------
   -- Subsystem_Reverse_Dependencies --
   ------------------------------------
   
   -- A given Subsystem name key maps to a set of all Subsystem names that
   -- depend on the given Subsystem. This is used to trace provide additional
   -- information to the user when Subsystems cannot be aquired
   
   package Subsystem_Reverse_Dependencies is new Protected_Map
     (Key_Type     => Unit_Names.Unit_Name,
      Element_Type => Unit_Names.Sets.Set,
      Maps         => Dependency_Maps);
      
   
end Registrar.Registry;
