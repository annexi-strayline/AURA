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

with Ada.IO_Exceptions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Characters.Conversions;
with Ada.Containers.Hashed_Sets;

with Platform_Info;
with Unicode.Case_Folding.Simple;
with Ada_Lexical_Parser;          use Ada_Lexical_Parser;

separate (Repositories)

procedure Validate_AURA_Spec 
  (Stream: not null access Ada.Streams.Root_Stream_Type'Class) 
is
   End_Error: exception renames Ada.IO_Exceptions.End_Error;
   
   Source: Source_Buffer (Stream);
   E: Lexical_Element;
   
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
   
   function To_String (Item      : in Wide_Wide_String;
                       Substitute: in Character := ' ')
                      return String
     renames Ada.Characters.Conversions.To_String;
   
   procedure Next_Element is
   begin
      loop
         E := Next_Element (Source);
         
         exit when E.Category /= Comment;
      end loop;
   end Next_Element;
      
   function Category return Lexical_Category is (E.Category);
   function Content  return Wide_Wide_String is (To_WWS (E.Content));
   
begin
   
   -- The AURA Spec is very specific and simple. We are strict about
   -- the checking we do
   
   Next_Element;
   Assert (Check => Category = Reserved_Word 
             and then Content = "package",
           Message => "AURA spec shall be a package spec without "
             &        "with clauses");
   
   Next_Element;
   Assert (Check => Category = Identifier 
             and then Content = "aura",
           Message => "AURA package spec file does not have correct "
             &        "package name.");
   
   Next_Element;
   
   declare
      No_Pure_Msg: constant String
        := "AURA package shall be declared Pure with an aspect_specification";
   begin
      Assert (Check => Category = Reserved_Word 
                and then Content = "with",
              Message => No_Pure_Msg);
      
      Next_Element;
      Assert (Check => Category = Identifier
                and then Content = "pure",
              Message => No_Pure_Msg);
   end;
   
   Next_Element;
   Assert (Check => Category = Reserved_Word
             and then Content = "is",
           Message => "Malformed package declaration");
   
   
   -- The package shall contain a single enumeration type declaration
   -- for Repository_Format
   
   declare
      Invalid_Declarations: constant String
        :=  "AURA package shall contain only the declaration for "
          & "enumeration type Repository_Format";
      
      procedure Assert (Check: in Boolean; 
                        Message: in String := Invalid_Declarations)
        renames Repositories.Assert;
   begin
      Next_Element;
      Assert (Category = Reserved_Word and then Content = "type");
      
      Next_Element;
      Assert (Category = Identifier and then Content = "repository_format");
      
      Next_Element;
      Assert (Category = Reserved_Word and then Content = "is");
      
      Next_Element;
      Assert (Category = Delimiter and then Content = "(");
              
   end;
   
   -- Now we can extract all the enumeration literals
   declare
      Expected_Literals   : Identifier_Sets.Set;
      Encountered_Literals: Identifier_Sets.Set;
   begin
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
      loop
         Next_Element;
         
         case Category is
            when Delimiter =>
               if Content = ")" then
                  Assert (Check   => Encountered_Literals.Length > 0,
                          Message => "Illegal enumeration");
                  exit;
                  
               else
                  Assert (Check   => Content = ",",
                          Message => "Illegal enumeration");
               end if;
               
            when Identifier =>
               begin
                  Encountered_Literals.Insert (E.Content);
               exception
                  when Constraint_Error =>
                     raise Assertion_Error with
                       "Illegal enumeration: Duplicate enumeration literals";
               end;
               
            when Comment =>
               null;
               
            when others =>
               raise Assertion_Error with "Illegal enumeration";
         end case;

      end loop;
      
      Next_Element;
      Assert (Check   => Category = Delimiter
                and then Content = ";",
              Message => "Illegal enumeration");
      
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
            
            for Format of Diff loop
               Append (Source => Unsupported_Formats,
                       New_Item => To_String (To_WWS (Format)));
            end loop;
            
            raise Assertion_Error with UBS.To_String (Unsupported_Formats);
         end if;
      end;
   end;
   
   -- Next we expect to find the platform infromation, in a particular order
   
   declare
      Have_Family, Have_Flavor, Have_Version, Have_Arch: Boolean := False;
      
      function Have_All return Boolean is
        (Have_Family and Have_Flavor and Have_Version and Have_Arch);
      
      package Expected_Values is
         use Ada.Characters.Conversions;
         
         Family: constant Wide_Wide_String := To_Wide_Wide_String
           (Platform_Info.Platform_Family);
         
         Flavor:  constant Wide_Wide_String := To_Wide_Wide_String
           (Platform_Info.Platform_Flavor);
         
         Version: constant Wide_Wide_String := To_Wide_Wide_String
           (Platform_Info.Platform_Version);
         
         Arch   : constant Wide_Wide_String := To_Wide_Wide_String
           (Platform_Info.Platform_Architecture);
      end Expected_Values;
      
      
      generic
         Name          : in Wide_Wide_String;
         Expected_Value: in Wide_Wide_String;
      procedure Verify_Value;
      
      -- Verify that the pattern following Name is a constant
      -- String declaration, with the correct value and style
      
      procedure Verify_Value is
      begin
         Next_Element;
         Assert (Check   => Category = Delimiter 
                   and then Content = ":",
                 Message => "expected "":"" following identifier """
                   & To_String (Name)
                   & """");
         Next_Element;
         Assert (Check   => Category = Reserved_Word 
                   and then Content = "constant",
                 Message => """" & To_String (Name)
                   & """ must be a constant");
         
         Next_Element;
         Assert (Check   => Category = Identifier
                   and then Content = "string",
                 Message => """" & To_String (Name)
                   & """ must be a String");
         
         Next_Element;
         Assert (Check   => Category = Delimiter
                   and then Content = ":=",
                 Message => "expected "":="" following ""String"" """
                   & To_String (Name)
                   & """");
         
         Next_Element;
         Assert (Check   => Category = String_Literal,
                 Message => "Expected String literal");
         
         Assert (Check   => Content = Expected_Value,
                 Message => "Expected value for """ 
                   & To_String (Name) & """ is """
                   & To_String (Expected_Value) & """");
         
         Next_Element;
         Assert (Check   => Category = Delimiter
                   and then Content = ";",
                 Message => "Expressions are not allowed");
      end Verify_Value;
      
      procedure Verify_Family is new Verify_Value 
        (Name           => "platform_family",
         Expected_Value => Expected_Values.Family);
      
      procedure Verify_Flavor is new Verify_Value 
        (Name           => "platform_flavor",
         Expected_Value => Expected_Values.Flavor);
      
      procedure Verify_Version is new Verify_Value 
        (Name           => "platform_version",
         Expected_Value => Expected_Values.Version);
      
      procedure Verify_Arch is new Verify_Value 
        (Name           => "platform_architecture",
         Expected_Value => Expected_Values.Arch);
      
   begin
      while not Have_All loop
         Next_Element;
         Assert (Check => Category = Identifier,
                 Message => "Unexpected """ 
                   &        To_String (Content)
                   &        """ in AURA package");
         
         if Content = "platform_family" then
            Verify_Family;
            Have_Family := True;
            
         elsif Content = "platform_flavor" then
            Verify_Flavor;
            Have_Flavor := True;
            
         elsif Content = "platform_version" then
            Verify_Version;
            Have_Version := True;
            
         elsif Content = "platform_architecture" then
            Verify_Arch;
            Have_Arch := True;
            
         else
            raise Assertion_Error with
              "Illegal identifier """ 
              & To_String (Content)
              & """ in AURA spec";
            
         end if;
      end loop;
   end;
   
   Next_Element;
   Assert (Check   => Category = Reserved_Word
             and then Content = "end",
           Message => "AURA package shall only contain the Repository_Format " 
             & "type declaration, and the Platform information constants.");
   
   Next_Element;
   
   if Category = Identifier then
      Assert (Check => Content = "aura",
              Message => "Package name mismatch - should be AURA");
      Next_Element;
   end if;
   
   Assert (Check => Category = Delimiter
             and then Content = ";",
           Message => "Syntax error at end of package - expected "";"" "
             & "following ""end""");
   
   -- Looks good.
   
end Validate_AURA_Spec;
