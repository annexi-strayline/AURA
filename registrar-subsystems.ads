------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
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

-- This package implements the Subsystem type for use in tracking Subsystem
-- dependencies for an AURA project when invoking the CLI. This type is 
-- intended to act as the Element_Type for the Hashed Set declared in the
-- Subsystems.Subsystem_Sets child package.

with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

with Unit_Names;
with Unit_Names.Hash;
with Unit_Names.Sets;
with Repositories;
with Registrar.Source_Files;

package Registrar.Subsystems is
   
   -- Subsystem_State --
   ---------------------
   -- The Subsystem_State pertains to each Subsystem referenced by withed 
   -- library units of a project. The Subsystem_State begings at Registered
   -- when entered into a Set, and will never regress. If a Subsystem can never
   -- reach Available, AURA fails
   
   type Subsystem_State is
     (Requested,
      -- Subsystem must be aquired and entered into the AURA project
      --
      -- Once all Subsystems in a Set have reached Registered, each Subsystem
      -- is checked for existence in the current project. If the Subsystem
      -- has been aquired previously, the Status for the Subsystem advances
      -- to Aquired. Otherwise, the Subsystem needs to be aquired, and is
      -- advanced to Requested.
      
      Aquired,
      -- Requested Subsystems must be aquired. Failure to aquire any Requested
      -- Subsystem causes AURA to fail.
      --
      -- Aquired means that the Subsystem's sources have been checked-out into
      -- the appropriate subdirectory, but that Configuration has not been
      -- completed. Units in the subsystem root directory have been entered,
      -- but special codepaths have not been, as this must happen after
      -- configuration
      
      Unavailable,
      -- Aquisition of the subsystem failed. The reason for this failure is
      -- assigned to the Aquisition_Note component (and thus only applies
      -- to AURA subsystems)
      
      Available);
      -- The subsystem has been Configured, and is now available for
      -- compilation
   
   
   -- Configuration_Pack --
   ------------------------
   -- The Configuration_Pack contains all of the important configuration
   -- parameters for configuration a subsystem for the local system.
   --
   -- These values are loaded from a configuration unit, and are a collection
   -- of named strings, where the names are just for readability of the
   -- configuration unit, but have no other significant meaning
   
   package WWU renames Ada.Strings.Wide_Wide_Unbounded;
   package UBS renames Ada.Strings.Unbounded;
   
   type Configuration_Pair is
      record
         Name : WWU.Unbounded_Wide_Wide_String;
         Value: UBS.Unbounded_String;
      end record;
   
   package Configuration_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Configuration_Pair);
   
   subtype Configuration_Vector is Configuration_Vectors.Vector;
   
   type Configuration_Pack is
      record
         External_Libraries: Configuration_Vector;
         Ada_Compiler_Opts : Configuration_Vector;
         C_Compiler_Opts   : Configuration_Vector;
         C_Definitions     : Configuration_Vector;
         Codepaths         : Configuration_Vector;
         Information       : Configuration_Vector;
      end record;
   
   
   ---------------
   -- Subsystem --
   ---------------
   
   use type Unit_Names.Unit_Name;
   
   type Subsystem (AURA: Boolean := False) is
     -- AURA = True  => the Subsystem is an AURA subsystem
     --        False => the Subsystem is part of the root project
      record
         Name : Unit_Names.Unit_Name;
         State: Subsystem_State := Requested;
         
         Configuration: Configuration_Pack;
         -- Mostly for AURA subsystems, however the root project also
         -- contains configuration, which is registered with the 
         -- "AURA" subsystem itself (which is a non AURA subsystem!)
         
         case AURA is
            when True =>
               Source_Repository : Repositories.Repository_Index;
               -- Checkout repository information
               
               Aquisition_Failure: UBS.Unbounded_String;
               -- Reason for failure to aquire, set by the checkout process,
               -- and valid only if State = Unavailable
               
            when False =>
               -- the Subsystem is part of the core project
               null;
         end case;
      end record;
   
   -- For implementation of Subsystems.Subsystem_Sets
   function Subsystem_Name_Hash (SS: Subsystem) return Ada.Containers.Hash_Type
     is (Unit_Names.Hash (SS.Name));
   
   function Equivalent_Subsystems (Left, Right: Subsystem) return Boolean
     is (Left.Name = Right.Name);
   
   --------------------
   -- Subsystem_Sets --
   --------------------
   
   package Subsystem_Sets is new Ada.Containers.Hashed_Sets 
       (Element_Type        => Subsystem,
        Hash                => Subsystem_Name_Hash,
        Equivalent_Elements => Equivalent_Subsystems);
   
end Registrar.Subsystems;

