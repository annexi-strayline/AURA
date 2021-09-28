------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

-- This package implements an extremely simply Ada (and gpr) lexical parser,
-- which utilized by the various other subsystems of the AURA CLI
--
-- This parser only handles parsing and categorizing each of the 7 types of
-- "lexical elements" defined by the Ada Reference Manual.

with Ada.IO_Exceptions;
with Ada.Characters.Latin_1;

with Unicode.UTF8_Stream_Decoder;
with Unicode.General_Category;     use Unicode.General_Category;
with Unicode.Case_Folding.Simple;
with Unicode.Normalization.Quick_Check.C;
with Unicode.Normalization.Quick_Check.KC;

package body Ada_Lexical_Parser is
   
   End_Error: exception renames Ada.IO_Exceptions.End_Error;
   
   -- Parse Infrastructure (ARM Definitions) ----------------------------------
   
   -- Normalization Form Quick Checks
   function Is_NFC  (C: Wide_Wide_Character) return Boolean 
   with Inline is
      use Unicode.Normalization.Quick_Check;
   begin
      return Unicode.Normalization.Quick_Check.C (C) in Yes | Maybe;
   end Is_NFC;
   
   
   function Is_NFKC (C: Wide_Wide_Character) return Boolean
   with Inline is
      use Unicode.Normalization.Quick_Check;
   begin
      return Unicode.Normalization.Quick_Check.KC (C) in Yes | Maybe;
   end Is_NFKC;
   
   -- Character definitions
   CHARACTER_TABULATION: constant Wide_Wide_Character 
     := Wide_Wide_Character'Val (16#09#);
   
   LINE_FEED: constant Wide_Wide_Character
     := Wide_Wide_Character'Val (16#0A#);
   
   LINE_TABULATION: constant Wide_Wide_Character
     := Wide_Wide_Character'Val (16#0B#);
   
   FORM_FEED: constant Wide_Wide_Character
     := Wide_Wide_Character'Val (16#0C#);
   
   CARRIAGE_RETURN: constant Wide_Wide_Character
     := Wide_Wide_Character'Val (16#0D#);
   
   NEXT_LINE: constant Wide_Wide_Character
     := Wide_Wide_Character'Val (16#85#);
   
   -- Query functions
   function Is_Legal_Outside_Comments (C: Wide_Wide_Character)
                                      return Boolean with Inline;
   -- RM 2.1(4/5)
   
   function Is_Format_Effector (C: Wide_Wide_Character)
                               return Boolean with Inline;
   
   function Is_Other_Control (C: Wide_Wide_Character)
                             return Boolean with Inline;
   
   function Is_Graphic_Character (C: Wide_Wide_Character)
                                 return Boolean with Inline;
   
   function Is_Digit (C: Wide_Wide_Character) return Boolean with Inline;
   
   function Is_Extended_Digit (C: Wide_Wide_Character)
                              return Boolean with Inline;
   -- Shall be called after simple case folding
   
   function Is_Numeral (S: Wide_Wide_String) return Boolean with Inline;
   
   function Is_Based_Numeral (S: Wide_Wide_String)
                             return Boolean with Inline;
   
   function Is_Delimiter (C: Wide_Wide_Character)
                         return Boolean with Inline;
   
   function Possible_Compound_Delimiter (C: Wide_Wide_Character)
                                        return Boolean with Inline;
   
   subtype Compound_Delimiter_String is Wide_Wide_String(1 .. 2);
   
   function Is_Compound_Delimiter (S: Compound_Delimiter_String)
                                  return Boolean with Inline;
   
   function Is_End_Of_Line (C: Wide_Wide_Character) 
                           return Boolean with Inline;
   
   function Is_Separator (C: Wide_Wide_Character)
                         return Boolean with Inline;
   
   function Is_Legal_Identifier (S: Wide_Wide_String) 
                                return Boolean with Inline;
   
   function Is_Reserved_Word (S: Wide_Wide_String)
                             return Boolean with Inline;
   
   -- Implementations
   function Is_Legal_Outside_Comments (C: Wide_Wide_Character)
                                      return Boolean
     is (Is_Graphic_Character (C) 
           or else Is_Format_Effector (C)
           or else General_Category (C) = Other_Format);
   
   function Is_Format_Effector (C: Wide_Wide_Character) return Boolean 
     is (General_Category (C) in Separator_Line | Separator_Paragraph
            or else C in   CHARACTER_TABULATION | LINE_FEED
            | LINE_TABULATION      | FORM_FEED
            | CARRIAGE_RETURN      | NEXT_LINE);
   
   function Is_Other_Control (C: Wide_Wide_Character) return Boolean
     is (General_Category (C) = Other_Control 
           and then not Is_Format_Effector (C));
   
   function Is_Graphic_Character (C: Wide_Wide_Character) return Boolean
     is (General_Category (C) not in Other_Private_Use | Other_Surrogate
           and then not Is_Format_Effector (C)
           and then not Is_Other_Control (C));
   -- Note that the UTF8 stream decoder already disallows the
   -- U+XXFFFE..U+XXFFFF ranges. The ARM explicitly calls for disallowing these
   -- ranges in Graphic_Character, but we've already done that with the
   -- UTF-8 decoding
   
   function Is_Digit (C: Wide_Wide_Character) return Boolean
     is (C in '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9');
   
   function Is_Extended_Digit (C: Wide_Wide_Character)
                              return Boolean
     is (Is_Digit(C) or else C in 'a' | 'b' | 'c'| 'd' | 'e' | 'f');
   
   function Is_Numeral (S: Wide_Wide_String) return Boolean
     is (Is_Digit (S(S'First)) 
           and then (for all I in S'First + 1 .. S'Last
                       => (Is_Digit(S(I)) 
                             or else (S(I) = '_' 
                                        and then Is_Digit (S(I - 1)))))
           and then (Is_Digit (S(S'Last))));
   -- RM 2.4.1-3
   
   function Is_Based_Numeral (S: Wide_Wide_String)
                             return Boolean
     is (Is_Extended_Digit (S(S'First)) 
           and then (for all I in S'First + 1 .. S'Last
                       => (Is_Extended_Digit(S(I)) 
                             or else (S(I) = '_' 
                                        and then Is_Extended_Digit 
                                          (S(I - 1)))))
           and then (Is_Extended_Digit (S(S'Last))));
   
   function Is_Delimiter (C: Wide_Wide_Character) return Boolean
     is (C in '&' | ''' | '(' | ')' | '*' | '+' | ',' | '-' | '.'
            | '/' | ':' | ';' | '<' | '=' | '>' | '@' | '[' | ']' | '|');
   
   
   function Possible_Compound_Delimiter (C: Wide_Wide_Character) return Boolean
     is (C in '=' | '.' | '*' | ':' | '/' | '>' | '<');

   
   function Is_Compound_Delimiter (S: Compound_Delimiter_String) return Boolean
     is (S in "=>" | ".." | "**" | ":=" | "/=" 
            | ">=" | "<=" | "<<" | ">>" | "<>");
   
   
   function Is_End_Of_Line (C: Wide_Wide_Character) return Boolean
     is (Is_Format_Effector (C) and then C /= CHARACTER_TABULATION);
   
   
   function Is_Separator (C: Wide_Wide_Character) return Boolean
     is (General_Category(C) in Separator_Space
           or else Is_Format_Effector (C));
           -- Is_Format_Effector implies Is_End_Of_Line also
   
   function Is_Legal_Identifier (S: Wide_Wide_String) return Boolean is 
      -- Ada 2020 RM 2.3-3/2 - 4.1/5
      Category, Prev_Category: General_Category_Type;
      
      function Valid_Start return Boolean is
        (Category in Letter_Uppercase | Letter_Lowercase 
           | Letter_Titlecase | Letter_Modifier 
           | Letter_Other     | Number_Letter)
      with Inline;
      
      function Valid_Extend return Boolean is
        (Category in Mark_Nonspacing | Mark_Spacing_Combining
           | Number_Decimal_Digit | Punctuation_Connector)
      with Inline;
      
   begin
      Category := General_Category (S(S'First));
      
      if not Valid_Start then
         return False;
      end if;
      
      for I in S'First + 1 .. S'Last loop
         Prev_Category := Category;
         Category := General_Category (S(I));
            
         if not (Valid_Start or else Valid_Extend) then
            return False;
         end if;
         
         -- RM 2.2-4/3
         if Prev_Category = Punctuation_Connector
           and then Prev_Category = Category
         then
            return False;
         end if;
         
         -- RM 2.2-4.1/5
         if not Is_NFKC (S(I)) then
            return False;
         end if;
      end loop;
      
      -- RM 2.2-4/3
      -- May not end with Punctuation_Connector
      return Category /= Punctuation_Connector;
      
   end Is_Legal_Identifier;
   
   
   function Is_Reserved_Word (S: Wide_Wide_String) return Boolean is
      -- Shall be called:
      -- 1. After Is_Legal_Identifier (S) = True, and
      -- 2. After simple case folding
   begin
      case S(S'First) is
         when 'a' =>
            -- Starting with 'a'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'b' =>
                  if S'Length < 3 then return False; end if;
                  
                  case S(S'First + 2) is
                     when 'o' =>
                        return S = "abort";
                     when 's' =>
                        return S in "abstract" | "abs";
                        
                     when others => return False;
                  end case;
               when 'c' =>
                  if S'Length < 3 then return False; end if;
                  
                  if S(S'First + 2) = 'c' then
                     return S in "access" | "accept";
                  else
                     return False;
                  end if;
               when 'l' => return S = "all";
               when 'n' => return S = "and";
               when 'r' => return S = "array";
               when 't' => return S = "at";
                  
               when others => return False;
            end case;
            
         when 'b' =>
            -- Starting with 'b'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'e' => return S = "begin";
               when 'o' => return S = "body";
                  
               when others => return False;
            end case;
            
         when 'c' =>
            -- Starting with 'c'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'a' => return S = "case";
               when 'o' => return S = "constant";
                  
               when others => return False;
            end case;
            
         when 'd' =>
            -- Starting with 'd'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'e' =>
                  if S'Length < 3 then return False; end if;
                  
                  if S(S'First + 2) = 'c' then
                     return S = "declare";
                  else
                     return S in "delay" | "delta";
                  end if;
               when 'i' => return S = "digits";
               when 'o' => return S = "do";
                  
               when others => return False;
            end case;
            
         when 'e' =>
            -- Starting with 'e'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'l' => return S in "else" | "elsif";
               when 'n' => return S in "end" | "entry";
               when 'x' => return S in "exception" | "exit";
                  
               when others => return False;
            end case;
            
         when 'f' => return S in "for" | "function";
            
         when 'g' => return S in "generic" | "goto";
            
         when 'i' =>
            -- Starting with 'i'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'f' => return S = "if";
               when 'n' => return S in "in" | "interface";
               when 's' => return S = "is";
                  
               when others => return False;
            end case;
            
         when 'l' => return S in "loop" | "limited";
         when 'm' => return S = "mod";
            
         when 'n' =>
            -- Starting with 'n'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'e' => return S = "new";
               when 'o' => return S = "not";
               when 'u' => return S = "null";
                  
               when others => return False;
            end case;
            
         when 'o' =>
            -- Starting with 'o'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'f' => return S = "of";
               when 'r' => return S = "or";
               when 't' => return S = "others";
               when 'u' => return S = "out";
               when 'v' => return S = "overriding";
                  
               when others => return False;
            end case;
            
         when 'p' =>
            -- Starting with 'p'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'a' => return S in "package" | "parallel";
               when 'r' =>
                  if S'Length < 3 then return False; end if;
                  
                  case S(S'First + 2) is
                     when 'a' => return S = "pragma";
                     when 'i' => return S = "private";
                     when 'o' => return S in "procedure" | "protected";
                        
                     when others => return False;
                  end case;
                  
               when others => return False;
            end case;
            
         when 'r' =>
            -- Starting with 'r'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'a' => return S in "raise" | "range";
               when 'e' =>
                  if S'Length < 3 then return False; end if;
                  
                  case S(S'First + 2) is
                     when 'c' => return S = "record";
                     when 'm' => return S = "rem";
                     when 'n' => return S = "renames";
                     when 'q' => return S = "requeue";
                     when 't' => return S = "return";
                     when 'v' => return S = "reverse";
                        
                     when others => return False;
                  end case;
               when others => return False;
            end case;
            
         when 's' =>
            -- Starting with 's'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'e' => return S in "select" | "separate";
               when 'o' => return S = "some";
               when 'u' => return S = "subtype";
               when 'y' => return S = "synchronized";
                  
               when others => return False;
            end case;
            
         when 't' =>
            -- Starting with 't'
            if S'Length < 2 then return False; end if;
            
            case S(S'First + 1) is
               when 'a' => return S in "tagged" | "task";
               when 'e' => return S = "terminate";
               when 'h' => return S = "then";
               when 'y' => return S = "type";
                  
               when others => return False;
            end case;
            
         when 'u' => return S in "until" | "use";
            
         when 'w' =>
            -- Starting with 'w'
            if S'Length < 2 then return False; end if;
            
            if S(S'First + 1) = 'h' then
               return S in "when" | "while";
            else
               return S = "with";
            end if;
            
         when 'x' => return S = "xor";
            
         when others => return False;
      end case;
   end Is_Reserved_Word;
   
   
   ------------------
   -- Next_Element --
   ------------------
   function Next_Element (Source: in out Source_Buffer)
     return Lexical_Element
   is separate;
   
end Ada_Lexical_Parser;
