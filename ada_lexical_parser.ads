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
--
-- Obviously this parser should be invoked from the start of the Ada source.

with Ada.Streams;
with Ada.Strings.Wide_Wide_Unbounded;

package Ada_Lexical_Parser is
   
   Invalid_Ada: exception;
   
   type Lexical_Category is
     (Delimiter,
      Identifier,
      Reserved_Word,
      Numeric_Literal,
      Character_Literal,
      String_Literal,
      Comment);
   
   package WWU renames Ada.Strings.Wide_Wide_Unbounded;
   
   type Lexical_Element is
      record
         Category: Lexical_Category;
         Content : WWU.Unbounded_Wide_Wide_String;
      end record;
   -- Note that Character and String literals will be stripped of the
   -- enclosing ''' or '"', respectivly
   
   type Source_Position is
      record
         Absolute_Position: Positive := 1;  -- Characters from start of stream
         
         Line             : Positive := 1;
         Column           : Positive := 1;
      end record;
     
   
   type Source_Buffer 
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
   is private;
   -- The source buffer is used to permit look-ahead capabilities
   -- when identifying compound delimiters
   
   ------------------
   -- Next_Element --
   ------------------
   
   function Next_Element (Source: in out Source_Buffer)
                         return Lexical_Element;
   -- Returns the next lexical element in the Source_File.
   --
   -- -- Explicit Raises --
   -- *  Invalid_Ada: an illegal condition in the source was found.
   --                 Position (Source) will reveal to position of the
   --                 offending character
   
   -------------------
   -- Last_Position --
   -------------------
   
   function Last_Position (Source: Source_Buffer) return Source_Position;
   -- Returns the position at of the first character of the last element 
   -- retrieved, or the last character to indicate invalid Ada
   -- (Invalid_Ada raised during a call to Next_Element)
   
   
private
   subtype Buffer_Count   is Natural      range 0 .. 10;
   subtype Buffer_Index   is Buffer_Count range 1 .. 10;
   
   type Stream_Buffer       is array (Buffer_Index) of Wide_Wide_Character;
   type Line_Column_History is array (Buffer_Index) of Positive;
   
   
   type Source_Buffer
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
      is record
         Content     : Stream_Buffer;
         Level       : Buffer_Count := 0;
         
         Next_Pos    : Source_Position;
         Last_Pos    : Source_Position;
         
         Line_History: Line_Column_History := (others => 1);
         Last_Line   : Buffer_Index := 1;
         -- We track the last column of each previous line for the
         -- maximum length of the buffer, so that we can backtrack the
         -- column position when going "up" a line
         --
         -- Line_History takes the form of a circular buffer. This should be 
         -- fine, since it should mean the backtracking can never actually
         -- cause the buffer to circle back ahead of itself
      end record;
   
   function Last_Position (Source: Source_Buffer) return Source_Position
     is (Source.Last_Pos);
     
end Ada_Lexical_Parser;
