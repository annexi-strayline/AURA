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

-- This package offers a generic tool for scanning the identifiers declare in
-- a package specification.
--
-- The result is a tree that represents all explicitly declared entities within
-- a package specification, as well as their defining name, kind, and any child
-- declarations.
--
-- The scanner is not strict, and will ignore more than a few syntax and
-- legality errors. It is intended that scanner sources will be passing through
-- an actual compiler at some later time.


with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Containers.Multiway_Trees;

with Ada_Lexical_Parser;
with Registrar.Library_Units;

package Specification_Scanner is
   
   package WWU renames Ada.Strings.Wide_Wide_Unbounded;
   use type Registrar.Library_Units.Library_Unit_Kind;
   
   Syntax_Error: exception;
   
   type Entity_Kind is (Type_Declaration,
                        Subtype_Declaration,
                        Object_Declaration,
                        Number_Declaration,
                        Subprogram_Declaration,
                        Expression_Function_Declaration,
                        Package_Declaration,
                        Exception_Declaration);

   -- Generally following ARM 3.1 with contractions
   
   type Declared_Entity is 
      record
         Name: WWU.Unbounded_Wide_Wide_String;
         Kind: Entity_Kind;
            
         Is_Generic    : Boolean := False;
         Is_Constant   : Boolean := False;
         Is_Renaming   : Boolean := False;
         Is_Anon_Access: Boolean := False;
         
         Subtype_Mark: WWU.Unbounded_Wide_Wide_String;
         Expression  : WWU.Unbounded_Wide_Wide_String;
         -- Expression does not include the terminating delimiter (;)
            
         Renamed_Entity_Name: WWU.Unbounded_Wide_Wide_String;

      end record;
   
   package Declaration_Trees is 
     new Ada.Containers.Multiway_Trees (Declared_Entity);
   
   procedure Scan_Package_Spec
     (Unit     : in     Registrar.Library_Units.Library_Unit;
      Unit_Tree:    out Declaration_Trees.Tree)
   with Pre => Unit.Kind = Registrar.Library_Units.Package_Unit;
   
   -- This takes a library_unit which shall be a library package. The
   -- specifcation is scanned, and a declaration tree is returned.
   -- If the source is malformed, Syntax_Error is raised
   
end Specification_Scanner;
