------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2023, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Characters.Conversions;
with Ada.Containers.Hashed_Sets;

separate (Repositories.AURA_Spec_Handling.Check_AURA_Spec)

procedure Check_Repository_Format is
   
   package WWU renames Ada.Strings.Wide_Wide_Unbounded;
   
   package Identifier_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => WWU.Unbounded_Wide_Wide_String,
      Hash                => WWU.Wide_Wide_Hash,
      Equivalent_Elements => WWU."=",
      "="                 => WWU."=");
   
   
   function To_WWS (Source: WWU.Unbounded_Wide_Wide_String)
                   return Wide_Wide_String
     renames WWU.To_Wide_Wide_String;
   
   function To_WWS (Item: in String) return Wide_Wide_String
     renames Ada.Characters.Conversions.To_Wide_Wide_String;
   
   Expected_Literals   : Identifier_Sets.Set;
   Encountered_Literals: Identifier_Sets.Set;
   
begin
   -- The package shall contain a single enumeration type declaration
   -- for Repository_Format
   
   declare
      Invalid_Declarations: constant String
        :=  "AURA package shall contain only the declaration for "
          & "enumeration type Repository_Format";
      
      procedure Check (Test: in Boolean; 
                       Fail_Message: in String := Invalid_Declarations)
        renames Check;
   begin
      Next_Element;
      Check (Category = Reserved_Word and then Content = "type");
      
      if not Correct then return; end if;
      
      Next_Element;
      Check (Category = Identifier and then Content = "repository_format");
      
      if not Correct then return; end if;
      
      Next_Element;
      Check (Category = Reserved_Word and then Content = "is");
      
      if not Correct then return; end if;
      
      Next_Element;
      Check (Category = Delimiter and then Content = "(");
      
   end;
   
   -- Attempt to extract all the enumeration literals

   -- load the expected literals from the internal literals of the type
   for Format in Repository_Format loop
      declare
         Img: Wide_Wide_String
           := To_WWS (Repository_Format'Image (Format));
      begin
         for C of Img loop
            C := Unicode.Case_Folding.Simple (C);
         end loop;
         
         Expected_Literals.Include 
           (WWU.To_Unbounded_Wide_Wide_String (Img));
      end;
   end loop;
   
   -- Now we load the literals from the spec
   while Correct loop
      Next_Element;
      
      case Category is
         when Delimiter =>
            if Content = ")" then
               Check (Encountered_Literals.Length > 0,
                      "Illegal enumeration");
               exit;
               
            else
               Check (Content = ",", "Illegal enumeration");
            end if;
            
         when Identifier =>
            begin
               Encountered_Literals.Insert (E.Content);
            exception
               when Constraint_Error =>
                  Check (False, "Illegal enumeration: " & 
                           "Duplicate enumeration literals");
            end;
            
         when Comment =>
            -- Next element will skip these
            null;
            
         when others =>
            Check (False, "Illegal enumeration");
            return;
      end case;
   end loop;
   
   if not Correct then return; end if;
   
   Next_Element;
   Check (Category = Delimiter and then Content = ";", "Illegal enumeration");
   
   if not Correct then return; end if;
   
   -- Finally compare the sets.
   declare
      use UBS;
      use Identifier_Sets;
      
      Diff: Identifier_Sets.Set := Difference 
        (Encountered_Literals, Expected_Literals);
      
      Unsupported_Formats: Unbounded_String
        := To_Unbounded_String ("Unsupported repository formats: ");
   begin
      if Difference (Encountered_Literals, 
                     Expected_Literals    ).Length > 0
      then
         Set_Unbounded_String 
           (Target => Unsupported_Formats,
            Source => "Unsupported repository formats: ");
         
         for Format of Literals_Diff loop
            Append (Source   => Unsupported_Formats,
                    New_Item => To_String (To_WWS (Format)));
         end loop;
         
         Check(Fail, To_String (Unsupported_Formats));
         -- Some inefficiencies here going via Check, but this is not
         -- really a performance-critical operation.
      end if;
   end;
   
end Check_Repository_Format;
