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

-- This package provides a standardized Unit/Subsystem name facility

-- All names going into a Unit_Name will be come from the Ada Lexical Parser,
-- and so will always have Unicode Simple Case Folding applied

with Ada.Strings.Wide_Wide_Unbounded;

package Unit_Names is
   
   type Unit_Name is tagged private;
   
   function  "<" (Left, Right: Unit_Name) return Boolean;
   -- Lexicographical
   
   function  Match_Initial (Name: Unit_Name; Initial: Wide_Wide_String)
                           return Boolean;
   -- Returns True if Initial matches the head portion of the name.
   --
   -- eg: Name => abcde, Initial => abcd   returns True
   --                    Initial => abcdef returns False
   
   function  Valid_Unit_Name (Candidate: in Wide_Wide_String) return Boolean;
   -- Returns True iff Candidate is a valid unit name, according to the RM, 
   -- which means an expanded name, or if the name is a properly formatted
   -- AURA "external unit" name.
   --
   -- Valid_Unit_Name is not a precondition to Set_Name because AURA constructs
   -- names while parsing, and often times they transition through invalid
   -- states.
   --
   -- Unit names sources externaly should be checked via this function whenever
   -- it is possible that the user could provide an invalid unit name
   
   function  Is_External_Unit (Name: Unit_Name) return Boolean with 
     Pre => Valid_Unit_Name (Name.To_String);
   -- Returns True if Name is for an "external unit"
   
   function  To_String      (Name: Unit_Name) return Wide_Wide_String;
   function  To_UTF8_String (Name: Unit_Name) return String;
   
   function  Set_Name (S: Wide_Wide_String) return Unit_Name;
   
   procedure Set_Name (Name:    out Unit_Name;
                       S   : in     Wide_Wide_String);
   
   -- Returns or Sets Name after applying Simple Case Folding to
   -- S.
   
   procedure Prepend (Name: in out Unit_Name; S: in Wide_Wide_String);
   procedure Append  (Name: in out Unit_Name; S: in Wide_Wide_String);
   -- Prepends/Appends S to the end of Unit_Name
   -- Mainly used when building a name during parsing.
   
   function  "&" (Left: Unit_Name; Right: Wide_Wide_String) return Unit_Name;
   function  "&" (Left: Wide_Wide_String; Right: Unit_Name) return Unit_Name;
   function  "&" (Left, Right: Unit_Name)                   return Unit_Name;
   
   function  Empty (Name: Unit_Name) return Boolean;
   
   function  Parent_Name (Name: Unit_Name) return Unit_Name;
   -- Returns the unit name for the parent of an Ada unit. If
   -- Name is already a top-level name (a subsystem), the returned
   -- Unit_Name is empty.
   --
   -- This subprogram should not be invoked on non-Ada unit names
   --
   -- E.g. for N: Name = "a.b.c.d":
   -- N.Parent_Unit = "a.b.c"
   --
   -- For N: Name = "p":
   -- N.Parent_Unit = ""
   
   function  Subsystem_Name (Name: Unit_Name) return Unit_Name;
   -- Extracts the Subsystem name from a full name. This simply means
   -- it extracts the identifier that consists of the first prefixed portion.
   --
   -- E.g. For N: Name := "a.b.c.d":
   -- N.Subsystem_Name = "a"
   --
   -- For N: Name := "p":
   -- N.Subsystem_Name = "p"
   --
   -- For the external dependency where N: Name := "p%binding.c"
   -- N.Subsystem_Name = "p"
   
   function Self_Direct_Name (Name: Unit_Name) return Unit_Name;
   -- Returns the direct name of Name from the perspective of that unit. I.e:
   --
   -- For N: Name := "a.b.c.d":
   -- N.Self_Direct_Name = "d"
   --
   -- For N: Name := "p":
   -- N.Self_Direct_Name = "p"
   --
   -- For N: Name := "p%binding.c"
   -- N.Self_Direct_Name = "binding.c"
   
private
   
   package WWU renames Ada.Strings.Wide_Wide_Unbounded;
   
   type Unit_Name is tagged
      record
         Name_String: WWU.Unbounded_Wide_Wide_String;
      end record;
   
end Unit_Names;
