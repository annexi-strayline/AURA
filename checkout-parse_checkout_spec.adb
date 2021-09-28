------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
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

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Characters.Conversions;
with Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Unbounded;

with Ada_Lexical_Parser; use Ada_Lexical_Parser;
with Registrar.Source_Files;

separate (Checkout)

function Parse_Checkout_Spec (Unit: Registrar.Library_Units.Library_Unit)
                             return Repositories.Repository_Index
is 
   use Registrar.Source_Files;
   use type Repositories.Repository_Index;
   
   Stream: aliased Source_Stream 
     := Checkout_Read_Stream (Unit.Spec_File);
   
   Source: Source_Buffer (Stream'Access);
   E: Lexical_Element;
   
   package WWU renames Ada.Strings.Wide_Wide_Unbounded;
   
   procedure Next_Element is
   begin
      loop
         E := Next_Element (Source);
         
         exit when E.Category /= Comment;
      end loop;
   end Next_Element;
   
   function Category return Lexical_Category is (E.Category);
   function Content  return Wide_Wide_String 
     is (WWU.To_Wide_Wide_String (E.Content));
   
   Repo_Index: Repositories.Repository_Index;
   
   function Read_Index return Repositories.Repository_Index is
      use WWU;
      
      Ident: constant Unbounded_Wide_Wide_String
        := To_Unbounded_Wide_Wide_String (Content);
      
      Lead_In: constant Wide_Wide_String := "repository_";
   begin
      -- Content should be an identifier that is expected to be
      -- "repository_xyz". We need to read in xyz
      Assert (Check => Slice (Source => Ident,
                              Low => 1,
                              High => Lead_In'Length)
                = Lead_In,
              Message => "Expected identifier of repository_nnn");
      
      declare
         use Ada.Characters.Conversions;
         
         package Index_IO is new Ada.Text_IO.Integer_IO
           (Repositories.Repository_Index);
         
         Number_Part: constant String
           := To_String (Slice (Source => Ident,
                                Low    => Lead_In'Length + 1,
                                High   => Length (Ident)));
         
         Last: Positive;
         Index: Repositories.Repository_Index;
      begin
         Index_IO.Get (From => Number_Part,
                       Item => Index,
                       Last => Last);
         
         Assert (Check   => Last = Number_Part'Last,
                 Message => "Repository index is too large");
         
         return Index;
         
      exception
         when Ada.Text_IO.Data_Error =>
            raise Ada.Assertions.Assertion_Error with
              "Repository index must be a number.";
      end;
      
   end Read_Index;
   
begin
   
   -- We expect exactly one with statement:
   -- with AURA.Repository_X;
   Next_Element;
   Assert (Check   => Category = Reserved_Word and then Content = "with",
           Message => "Expected with");
   
   Next_Element;
   Assert (Check   => Category = Identifier and then Content = "aura",
           Message => "With shall be AURA.Repository_X only");
   
   Next_Element;
   Assert (Check   => Category = Delimiter and then Content = ".",
           Message => "Expected '.'");
   
   Next_Element;
   Assert (Check   => Category = Identifier,
           Message => "Identifier expected");
   
   Repo_Index := Read_Index;
   Next_Element;
   Assert (Check   => Category = Delimiter and then Content = ";",
           Message => "';' expected");
   
   
   
   Next_Element;
   Assert (Check   => Category = Reserved_Word and then Content = "package",
           Message => "Expected package");
   
   -- Note that we don't need to actually check the package identifier, as this
   -- was handled by the unit entry logic (the name of Unit itself is derrived
   -- from parsing the identifier), we just want to specifically see what is
   -- after "renames" (it must be the same as the withed repository)
   
   loop
      Next_Element;
      
      exit when Category = Reserved_Word and then Content = "renames";
   end loop;
   
   -- "AURA." is valid next, but typically not what we find - but it is legal
   
   Next_Element;
   Assert (Check => Category = Identifier,
           Message => "Expected identifier");
   
   if Content = "AURA" then
      Next_Element;
      Assert (Check => Category = Delimiter and then Content = ".",
              Message => "Expected repository package identifier after "
                & "'AURA.'");
      
      -- We now should be at the repositoy identifier
      Next_Element;
      Assert (Check   => Category = Identifier,
              Message => "Expected repository package identifier");
   end if;
   
   declare
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      Assert (Check => Read_Index = Repo_Index,
              Message => "Checkout rename mismatch. Expected " &
                "Repository_" 
                & Trim 
                  (Source => Repositories.Repository_Index'Image (Repo_Index),
                   Side   => Both));
   end;
   
   -- Lastly a closing delimiter
   Next_Element;
   Assert (Check   => Category = Delimiter and then Content = ";",
           Message => "Expected ';'");
   
   return Repo_Index;
   
exception
   when e: Ada.Assertions.Assertion_Error =>
      declare
         Pos: Source_Position := Last_Position (Source);
      begin
         raise Ada.Assertions.Assertion_Error with
           Unit.Spec_File.Full_Name
           & ":" & Positive'Image (Pos.Line) 
           & ":" & Positive'Image (Pos.Column)
           & " - " & Ada.Exceptions.Exception_Information (e);
      end;
   
end Parse_Checkout_Spec;
