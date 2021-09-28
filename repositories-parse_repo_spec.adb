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

with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions;

with Unicode.Case_Folding.Simple;
with Ada_Lexical_Parser; use Ada_Lexical_Parser;

separate (Repositories)

procedure  Parse_Repo_Spec 
  (Stream       : not null access Ada.Streams.Root_Stream_Type'Class;
   Expected_Name: in     Unit_Names.Unit_Name;
   Repo         :    out Repository)
is
   Source: Source_Buffer (Stream);
   E: Lexical_Element;
   Peeked: Boolean := False;
   -- When True, the next call to Next_Element has no effect, and resets
   -- Peeked to false
   
   package WWU renames Ada.Strings.Wide_Wide_Unbounded;
   
   function To_WWS (Source: WWU.Unbounded_Wide_Wide_String)
                   return Wide_Wide_String
     renames WWU.To_Wide_Wide_String;
   
   function To_WWS (Item: in String) return Wide_Wide_String
     renames Ada.Characters.Conversions.To_Wide_Wide_String;
   
   function To_String (Item      : in Wide_Wide_String;
                       Substitute: in Character := ' ')
                      return String
     renames Ada.Characters.Conversions.To_String;
   
   procedure Next_Element (Peek: in Boolean := False) is
   begin
      -- Peek essentially just ensures the next call to Next_Element
      -- leaves E as is, for that call. This allows a call to Next_Element
      -- that does not effect the next call to Next_Element
      
      if Peeked then
         -- If we are doing another Peek, just leave it as is
         if not Peek then
            Peeked := False;
         end if;
         
      else
         loop
            E := Next_Element (Source);
            
            exit when E.Category /= Comment;
         end loop;
         
         if Peek then
            Peeked := True;
         end if;
      end if;
   end Next_Element;
   
      
   function Category return Lexical_Category is (E.Category);
   function Content  return Wide_Wide_String is (To_WWS (E.Content));
   
   function Read_Constant (Name          : in Wide_Wide_String;
                           Subtype_Mark  : in Wide_Wide_String;
                           Value_Category: in Lexical_Category;
                           Optional      : in Boolean := False)
                          return Wide_Wide_String 
   is
   begin
      Next_Element (Peek => Optional);
      
      if Optional then 
         if Category /= Identifier
           or else Content /= Name
         then
            -- Not a match for an optional declaration
            return "";
         else
            -- Clear the peek
            Next_Element;            
         end if;
      end if;
      
      Assert (Check   => Category = Identifier
                   and then Content = Name,
                 Message => "Expected declaration of """
                   &        To_String (Name) & '"');
      
      Next_Element;
      Assert (Check => Category = Delimiter
                and then Content = ":",
              Message => """:"" expected.");
      
      Next_Element;
      Assert (Check => Category = Reserved_Word
                and then Content = "constant",
              Message => "All object declarations shall be constant");
      
      Next_Element;
      Assert (Check => Category = Identifier
                and then Content = Subtype_Mark,
              Message => "Incorrect subtype for """
                &        To_String (Name) & """. Should be """
                &        To_String (Subtype_Mark) & '"');
      
      Next_Element;
      Assert (Check => Category = Delimiter
                and then Content = ":=",
              Message => """:="" expected.");
      
      Next_Element;
      Assert (Check => Category = Value_Category,
              Message => "Invalid value for assignment");
      
      return Value: Wide_Wide_String := Content do
         -- Ensure that a ';' follows. We don't allow expressions
         -- for these assignments
         Next_Element;
         Assert (Check => Category = Delimiter
                   and then Content = ";",
                 Message => "Expressions are not permitted in Repository "
                   &        "specifications.");
      end return;
   end Read_Constant;
   
begin
   
   Next_Element;
   Assert (Check   => Category = Reserved_Word
             and then Content = "package",
           Message => "Repository specifications must be packages, and "
             & "shall not have any with statements");
   
   Next_Element;
   Assert (Check   => Category = Identifier
             and then Content = "aura",
           Message => "Package name incorrect for this repository. "
             &        "Expected: " & Expected_Name.To_UTF8_String);
   
   Next_Element;
   Assert (Check   => Category = Delimiter and then Content = ".",
           Message => "Package name incorrect for this repository. "
             &        "Expected: " & Expected_Name.To_UTF8_String);
           
   
   Next_Element;
   Assert (Check   => Category = Identifier
             and then ("aura." & Content) = Expected_Name.To_String,
           Message => "Package name incorrect for this repository. "
             &        "Expected: " & Expected_Name.To_UTF8_String);
   
   Next_Element;
   Assert (Check   => Category = Reserved_Word
             and then Content = "with",
           Message => "Expected ""with"" - "
             &        "Repository specifications must be Pure");
   
   Next_Element;
   Assert (Check   => Category = Identifier
             and then Content = "pure",
           Message => "Expected ""Pure"" - "
             &        "Repository specifications must be Pure");
   
   Next_Element;
   Assert (Check => Category = Reserved_Word
             and then Content = "is",
           Message => "Expected ""is""");
   
   -- Load format and initialize the record
   declare
      Format_Value: Wide_Wide_String
        := Read_Constant (Name           => "format",
                          Subtype_Mark   => "repository_format",
                          Value_Category => Identifier);
      Found_Match: Boolean := False;
      Format: Repository_Format;
   begin
      -- Match the value. This method ensures that if we change the
      -- definition of Repository_Format, we don't need to change
      -- anything here
      for F in Repository_Format loop
         declare
            Check_Value_S: constant String 
              := Repository_Format'Image (F);
            Check_Value: Wide_Wide_String
              := To_WWS (Check_Value_S);
         begin
            for C of Check_Value loop
               C := Unicode.Case_Folding.Simple (C);
            end loop;
            
            if Format_Value = Check_Value then
               Found_Match := True;
               Format := F;
               exit;
            end if;
         end;
      end loop;
      
      Assert (Check   => Found_Match,
              Message => '"' 
                & Ada.Characters.Conversions.To_String (Format_Value)
                & """ is not a valid value for Repository_Format");
      
      -- Initialize the Repo record with the Format discriminent
      -- Languages rules mean we cant initialize a discriminent to a non-
      -- static value
      Repo := (case Format is
                  when System => (Format => System, others => <>),
                  when Local  => (Format => Local,  others => <>),
                  when Git    => (Format => Git,    others => <>));
      
      
   end;
   
   -- Next-up is "Location", which is present for all formats
   UBS.Set_Unbounded_String 
     (Target => Repo.Location,
      Source => Ada.Characters.Conversions.To_String 
        (Read_Constant (Name           => "location",
                        Subtype_Mark   => "string",
                        Value_Category => String_Literal)));
   
   UBS.Set_Unbounded_String
           (Target => Repo.Snapshot,
            Source => Ada.Characters.Conversions.To_String
              (Read_Constant (Name           => "snapshot",
                              Subtype_Mark   => "string",
                              Value_Category => String_Literal,
                              Optional       => True)));
   
   case Repo.Format is
      when System | Local =>
         null;
         
      when Git =>
         -- Also load snapshot and Tracking_Branch (if they exist).
         -- They must appear in that order, and there must be at least
         -- one
         
         UBS.Set_Unbounded_String
           (Target => Repo.Tracking_Branch,
            Source => Ada.Characters.Conversions.To_String
              (Read_Constant (Name           => "tracking_branch",
                              Subtype_Mark   => "string",
                              Value_Category => String_Literal,
                              Optional       => True)));
         
         Assert (Check => (if UBS.Length (Repo.Snapshot) = 0 then
                              UBS.Length (Repo.Tracking_Branch) > 0),
                 Message => "There must be either a Snapshot or "
                   & "Tracking_Branch value for a git repository.");
         
   end case;
   
   -- Finally, there should not be any more unrecognized object declarations,
   -- so we expect to see "end Repository_X;"
   
   Next_Element;
   Assert (Check => Category = Reserved_Word
             and then Content = "end",
           Message => "There shall not be any other declarations in "
             &        "a repository specification");
   
   Next_Element;
   
   if Category = Identifier then
      Assert (Check   => Content = "aura",
              Message => "Package name mismatch at end. Expected """
                & Expected_Name.To_UTF8_String
                & """, found """
                & Ada.Characters.Conversions.To_String (Content)
                & '"');
      
      Next_Element;
      Assert (Check   => Category = Delimiter and then Content = ".",
              Message => "Package name mismatch at end. Expected """
                & Expected_Name.To_UTF8_String
                & """, found """
                & Ada.Characters.Conversions.To_String (Content)
                & '"');
      
      Next_Element;
      Assert (Check   => Category = Identifier 
                and then ("aura." & Content) = Expected_Name.To_String,
              Message => "Package name mismatch at end. Expected """
                & Expected_Name.To_UTF8_String
                & """, found """
                & Ada.Characters.Conversions.To_String (Content)
                & '"');
      Next_Element;
   end if;
   
   Assert (Check   => Category = Delimiter and then Content = ";",
           Message => "Expected "";""");
   
exception
   when e: Assertion_Error =>
      declare
         Pos: constant Source_Position := Last_Position (Source);
      begin
         raise Assertion_Error with 
           (Expected_Name.To_UTF8_String
              & ":" & Positive'Image (Pos.Line) 
              & ":" & Positive'Image (Pos.Column)
              & " - " & Ada.Exceptions.Exception_Information (e));
      end;
      
      
   when others =>
      raise;
      
end Parse_Repo_Spec;
