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

separate (Ada_Lexical_Parser)

function Next_Element (Source: in out Source_Buffer)
                      return Lexical_Element
is
   Buffer: Wide_Wide_String (1 .. 201);
   Last  : Natural := 0;
   -- Ada standard says that the compiler shall be able to handle lengths of
   -- at least 200 (2020 RM 2.2-14)
   
   function Decode_Next
     (UTF8_Stream: not null access Ada.Streams.Root_Stream_Type'Class
        := Source.Stream)
     return Wide_Wide_Character
     renames Unicode.UTF8_Stream_Decoder.Decode_Next;
   
   function To_Unbounded_WWS (Source: in Wide_Wide_String) 
                             return WWU.Unbounded_Wide_Wide_String
     renames WWU.To_Unbounded_Wide_Wide_String;
   
   -- Current_Position --
   ----------------------
   -- Returns the position of the character immediately before "Next_Pos",
   -- used to give the correct starting position for a Lexical Element.
   -- and also to adjust Next_Pos on Push_Source.
   --
   -- Consume_History is only used by Push when Current_Position is being
   -- used to set Next_Pos, and so Source.Last_Line should actually be
   -- decremented when necessary
   function Current_Position (Consume_History: Boolean := False)
                             return Source_Position 
   with Inline is
   begin
      return Cur_Pos: Source_Position := Source.Next_Pos do
         if Cur_Pos.Column = 1 then
            declare
               Last_Line: Positive;
            begin
               if Source.Last_Line = Source.Line_History'First then
                  Last_Line := Source.Line_History'Last;
               else
                  Last_Line := Source.Last_Line - 1;
               end if;
               
               Cur_Pos.Line := Cur_Pos.Line - 1;
               Cur_Pos.Column := Source.Line_History(Last_Line);
               
               if Consume_History then
                  Source.Last_Line := Last_Line;
               end if;
            end;
         else
            Cur_Pos.Column := Cur_Pos.Column - 1;
            
         end if;
         
         Cur_Pos.Absolute_Position := Cur_Pos.Absolute_Position - 1;
         
      end return;
   end Current_Position;
   
   
   -- Push_Source --
   -----------------
   -- Push_Source is for when we've looked too far into the buffer, and
   -- need to back-up.
   procedure Push_Source (C: Wide_Wide_Character) with Inline is
   begin
      -- This should never be able to cause the loss of any characters
      if Source.Level = Buffer_Count'Last then
         raise Storage_Error with "Source buffer is insufficient";
      end if;
      
      Source.Level := Source.Level + 1;
      Source.Content(Source.Level) := C;
      
      -- Backtrack Next_Pos
      Source.Next_Pos := Current_Position (Consume_History => True);
      
   end Push_Source;
   
   
   -- Pop_Source --
   ----------------
   -- Pop_Source is always used to pull the next character from the stream,
   -- if Push_Source put characters back, those will be given first. If the
   -- buffer is empty, then new characters will be pulled from the stream
   function Pop_Source (In_Comment: Boolean := False) 
                       return Wide_Wide_Character 
   is
   begin
      return C: Wide_Wide_Character do
         if Source.Level = 0 then
            C := Decode_Next;
         else
            C := Source.Content(Source.Level);
            Source.Level := Source.Level - 1;
         end if;
         
         -- Advance Next_Pos
         
         -- Next character is column 1 on a new line if we just popped an
         -- End_Of_Line indicator
         if Is_End_Of_Line (C) then
            -- Next item at new line, first column. We need to save the
            -- current column before resetting Column
            if Source.Last_Line = Source.Line_History'Last then
               -- Wrap-around
               Source.Last_Line := Source.Line_History'First;
            else
               Source.Last_Line := Source.Last_Line + 1;
            end if;
            
            Source.Line_History(Source.Last_Line) := Source.Next_Pos.Column;
            
            Source.Next_Pos.Column := 1;
            Source.Next_Pos.Line   := Source.Next_Pos.Line + 1;
         else
            Source.Next_Pos.Column := Source.Next_Pos.Column + 1;
         end if;
         
         Source.Next_Pos.Absolute_Position
           := Source.Next_Pos.Absolute_Position + 1;
         
         -- Check basic legality 
         if not Is_NFC (C) then
            raise Invalid_Ada with
              "Source is not in Normalization Form C. "
              & "(Ada 2020 RM 2.1-4.1/5)";
         end if;
         
         if not In_Comment 
           and then not Is_Legal_Outside_Comments (C)
         then
            raise Invalid_Ada with
              "Illegal character (Codepoint"
              & Natural'Image(Wide_Wide_Character'Pos (C))
              & ") (Ada 2020 RM 2.1-4/5)";
         end if;
      end return;
   end Pop_Source;
   
   
   -- Skip_Separators --
   ---------------------
   -- Separators are allowed between all lexical elements, and have no
   -- meaning, we will skip them at the start and enf of our search for
   -- Next_Element. Doing that at the end allows for the graceful detection
   -- of the legal end of the source.
   procedure Skip_Separators (Char_Buffer: in out Wide_Wide_Character)
   with Inline is
   begin
      -- Skip_Separators is intended to be called on a fetched character which
      -- is expected to be a location where separators are allowed. Therefore 
      -- due to RM 2.2-7.1/3, Other_Format is also allowed.
      
      -- After calling this, Char_Buffer contains a non-separator character,
      -- or else End_Error will be raised by Pop_Source.
      
      while Is_Separator (Char_Buffer)
        or else General_Category (Char_Buffer) = Other_Format 
      loop
         Char_Buffer := Pop_Source;
      end loop;
      
   end Skip_Separators;
   
   
   -- Specifically to call Skip_Separators on a discard buffer to see if there
   -- are any trailing separators
   procedure Skip_Trailing_Separators with Inline is
      C: Wide_Wide_Character;
      
   begin
      C := Pop_Source;
      Skip_Separators (C);
      Push_Source (C);
      
   exception
      when Ada.IO_Exceptions.End_Error =>
         -- Nothing after this group of separators
         -- (or maybe no separators at all!)
         null;
   end Skip_Trailing_Separators;
   
begin
   
   -- Start by loading in the next character of the source into position 1
   -- of the buffer
   Buffer(1) := Pop_Source;
   
   -- Skip any leading separators / Other_Format (RM 2.2-7.1/3), but also
   Skip_Separators (Buffer(1));
   
   -- Store Last_Pos at this point, since we are expecting that we're 
   -- looking at the start of a lexical element, which we will want to
   -- be able to pinpoint for the user. Whenever we find an illegal
   -- condition, we will adjust it accordingly
   Source.Last_Pos := Current_Position;
   
   
   -- In order of the RM, we look for
   -- 1. Delimiters (2.2-9/5 and 2.2-10)
   -- 2. Identifiers (2.3) (and Reserved words 2.9)
   -- 4. Numeric Literals (2.4)
   -- 5. Character Literals (2.5)
   -- 6. String Literals (2.6)
   -- 7. Comments
   if Is_Delimiter (Buffer(1)) then
      -- Try a compound delimiter
      if Possible_Compound_Delimiter (Buffer(1)) then
         
         -- Load one more for a look-ahead
         Buffer(2) := Pop_Source;
         
         if Is_Compound_Delimiter (Buffer(1 .. 2)) then
            Skip_Trailing_Separators;
            return (Category => Delimiter,
                    Content  => To_Unbounded_WWS (Buffer(1 .. 2)));
         else
            -- Read-ahead is not needed, so we put it back in the buffer
            Push_Source (Buffer(2));
            Last := 1;
            
            Skip_Trailing_Separators;
            return (Category => Delimiter,
                    Content  => To_Unbounded_WWS (Buffer(1 .. 1)));
         end if;
         
         
      else
         -- Check for a comment, otherwise we will mistake this as a '-'
         -- delimiter
         if Buffer(1) = '-' then
            -- Load one more for the look-ahead
            Buffer(2) := Pop_Source;
            
            if Buffer(2) = '-' then
               -- We have a comment. We load everything until we reach the
               -- end of the line (we also stip the leading "--"
               for I in Buffer'Range loop
                  -- End of file here is not illegal!
                  begin
                     Buffer(I) := Pop_Source (In_Comment => True);
                  exception
                     when End_Error => exit;
                     when others    => raise;
                  end;
                  
                  exit when Is_End_Of_Line (Buffer(I));
                  
                  Last := I;
               end loop;
               
               -- Look for buffer limitation
               if Last = Buffer'Last then
                  raise Invalid_Ada with
                    "Comment line exceeds" & Natural'Image (Last)
                    & " characters. The Ada RM does not require an "
                    & "implementation to handle such length. "
                    & "This source is rejected.";
               end if;
               
               Skip_Trailing_Separators;
               return
                 (Category => Comment,
                  Content  => To_Unbounded_WWS (Buffer (1 .. Last)));
            else
               -- False alarm
               Push_Source (Buffer(2));
               
            end if;
            
            -- Check for a possible character literal, which is not a "tick"
            -- delimiter!
         elsif Buffer(1) = ''' then
            -- The rules are clear (Ada 2020 RM 2.5), a legal character
            -- literal is always exactly 3 characters long
            Buffer(2) := Pop_Source;
            Buffer(3) := Pop_Source;
            
            if Buffer(3) = ''' then
               -- Bingo.
               
               if not Is_Graphic_Character (Buffer(2)) then
                  raise Invalid_Ada with
                    "Character literals shall be graphic_characters "
                    & "(RM 2.5-2)";
               end if;
               
               Skip_Trailing_Separators;
               return
                 (Category => Character_Literal,
                  Content  => To_Unbounded_WWS (Buffer(2 .. 2)));
            else
               -- We looked ahead too far
               Push_Source (Buffer(3));
               Push_Source (Buffer(2));
            end if;
         end if;
         -- If we make it here, this is regular delimiter
         
         Skip_Trailing_Separators;
         return (Category => Delimiter,
                 Content  => To_Unbounded_WWS (Buffer(1 .. 1)));

      end if;
   end if;
   
   -- We get here if any only if we are not looking at a delimiter, comment,
   -- or character literal
   
   -- As much as possible, we follow the order set-out in the RM (clause 2),
   -- so we now check for an identifier. 
   
   -- If it not an identifier, (due to the rules) it may must then be
   -- a literal of some kind, or else it is illegal.
   
   -- First check that the first character is a legal beginning of an
   -- Identifier
   if Is_Legal_Identifier (Buffer(1 .. 1)) then
      -- Load in all characters until we get to a separator or delimiter
      -- This may include many illegal identifier characters, but those
      -- will be illegal!
      
      Last := 1;
      for I in 2 .. Buffer'Last loop
         
         -- End of line here is not "technically" illegal (lexically),
         -- but is almost certainly illegal symantically. That is not
         -- our jurisdiction, however.
         begin
            Buffer(I) := Pop_Source;
         exception
            when End_Error => exit;
            when others    => raise;
         end;
         
         -- According to the RM 2.2-7, a separator is required between
         -- an identifier and an adjacent identifier/reserved word, numeric
         -- or literal. Obviously two identifiers without a separator would
         -- just look like a single identifier. Similarily, a numeric
         -- literal could be part of the identifier, unless it included
         -- a '#', which would be illegal in an identifier
         
         -- This means that we should stop when we find the beginning of any
         -- of the following lexical elements:
         -- 1. A delimiter
         -- 2. A character literal (handled by 1 since ''' is a delimiter
         --    also)
         -- 3. A string literal ('"'), or
         
         -- We are doing this all by look-ahead, but the numeric literal
         -- logic will also ensure it is followed by something appropriate
         -- as well (not an identifier/reserved word or numeric literal).
         --
         -- This will have the effect of ensuring that the previous element
         -- has already been separated from this one if required
         
         exit when Is_Separator (Buffer(I)) 
           or else Is_Delimiter (Buffer(I))
           or else Buffer(I) = '"';
         -- Note that we check specifically for Is_Separator, and not
         -- "Other_Format" (see RM 2.2-7.1/3). This is because any
         -- "Other_Format" before a separator means it would be part of
         -- the identifier (and thus illegal) - see AI05-0079-1
         
         Last := I;
      end loop;
      
      -- Check for a buffer limitation
      if Last = Buffer'Last then
         raise Invalid_Ada with
           "Identifier too long: Identifier longer than"
           & Natural'Image (Buffer'Last)
           & " is not required to be accepted by the Ada standard";
         
      elsif not Is_Separator (Buffer(Last + 1)) then
         -- Anything except for a separator at Last + 1 needs to be pushed
         -- back to the buffer
         Push_Source (Buffer(Last + 1));
      end if;
      
      -- Now we are ready to check the legality or reserved status of this
      -- apparent identifier
      if not Is_Legal_Identifier (Buffer(1 .. Last)) then
         raise Invalid_Ada with "Illegal identifier";
      end if;
      
      -- Now we can finally apply case folding and check for reserved status
      for C of Buffer(1 .. Last) loop
         C := Unicode.Case_Folding.Simple (C);
      end loop;
      
      -- Finally - reserved or not?
      return E: Lexical_Element do
         declare
            Content: Wide_Wide_String renames Buffer(1 .. Last);
         begin
            E.Content := To_Unbounded_WWS (Content);
            
            if Is_Reserved_Word (Content) then
               E.Category := Reserved_Word;
            else
               E.Category := Identifier;
            end if;
         end;
         
         Skip_Trailing_Separators;
      end return;
      
   end if;
   -- Getting here means: not an identifier or Reserved Word. Next-up are
   -- numeric literals (2.4). These consist of Decimal Literals and
   -- Based Literals
   
   -- The first character of every numeric literal is always a Digit
   -- (RM 2.4.1-4.1/2)
   if Is_Digit (Buffer(1)) then
      -- The bulk of the work here is verifying the legality of the
      -- presentation.
      
      -- We will load it one chunk at a time, handling bases, decimals,
      -- and exponents as we find them.
      
      -- For any given numeric literal, we should find only the following
      -- non-numeral characters in the literal
      -- 1. a '.' indicating a decimal real number
      --    -> Could be a compound delimiter "..", otherwise,
      --    -> Only 1 (2.4.1-2, 2.4.2-2)
      --    -> shall be inside '#'s of a based literal (2.4.2-2)
      -- 2. a '#' indicating a based literal
      --    -> Must have exactly 2, (2.4.2-2)
      --    -> must come after at least one numeral (2.4.2-2)
      --    -> Must demarcate at least one based numeral (2.4.2-2)
      -- 3. a 'e' indicating an exponent
      --    -> only 1 (2.4.1-2, 2.4.2-2)
      --    -> Must follow '#' if based (2.4.2-2), 
      --       or a numeral if decimal (2.4.1-2)
      --    -> Must be followed by at least one numeral,
      --       or else '+' or '-' (2.4.1-4)
      -- 3. a separator indicating the end of the literal
      -- 4. a delimiter indicating the end of the literal
      
      -- Based on these rules, we can set-up some variables and loop through
      -- until the end, detecting illegalities along the way.
      declare
         Special    : Boolean := False;
         Is_Real    : Boolean := False;
         Is_Based   : Boolean := False;
         Inside_Hash: Boolean := False;
         In_Exponent: Boolean := False;
         Got_Sign   : Boolean := False;
         Need_More  : Boolean := False;
         
         Decimal_At    : Positive; -- Index of the decimal point
         First_Hash    : Positive; -- Index of first '#'
         Last_Hash     : Positive; -- Index of last '#'
         Exponent_Mark : Positive; -- Index of end of exponent's 'e'
         Exponent_Sign : Positive; -- Index of exponents sign (if any),
                                   -- or equal to Exponent_Mark
      begin
         Last := 1;
         for I in 2 .. Buffer'Last loop
            begin
               Buffer(I) := Pop_Source;
            exception
               when End_Error =>
                  if Need_More then
                     raise Invalid_Ada with
                       "Unexpected end of source during numeric literal";
                  else
                     exit;
                  end if;
               when others => raise;
            end;
            
            -- Separators are always the end of the literal
            exit when Is_Separator (Buffer(I));
            
            -- If we find a delimiter, we need to make sure we push it
            -- back to the buffer before exiting
            if Is_Delimiter (Buffer(I)) then
               
               -- A few delimiters are part of the numeric literal syntax
               if Buffer(I) not in '.' | '-' | '+' then
                  -- These are definate delimiters ending the literal
                  Push_Source (Buffer(I));
                  exit;
                  
               elsif Buffer(I) in '.' | '-' then
                  -- These could be part of the literal, unless they are
                  -- compound delimiters (either a range or comment)
                  declare
                     T: Wide_Wide_Character;
                  begin
                     -- Just a peak!
                     T := Pop_Source;
                     Push_Source (T);
                     
                     if T = Buffer(I) then
                        -- Valid compound delimiter.
                        Push_Source (Buffer(I));
                        exit;
                     end if;
                     
                  exception
                     when End_Error =>
                        raise Invalid_Ada with
                          "Unexpected end of source during "
                          & "numeric literal";
                     when others => raise;
                  end;
                  
               end if;
            end if;
            
            -- Still in the literal
            if Inside_Hash then
               Buffer(I) := Unicode.Case_Folding.Simple (Buffer(I));
               
               Special := not (Is_Extended_Digit (Buffer(I)) 
                                 or else Buffer(I) = '_');
               -- This doesn't detect invalid numerals like, say '__AB_',
               -- but that is checked after the loop.
               
            else
               Special := not (Is_Digit (Buffer(I))
                                 or else Buffer(I) = '_');
            end if;
            
            Need_More := False;
            
            if Special then
               if not Inside_Hash then
                  -- Case folding for exponent 'e'
                  Buffer(I) := Unicode.Case_Folding.Simple (Buffer(I));
               end if;
               
               -- Non-digit, so we need to interpret their meaning
               case Buffer(I) is
                  when '.' =>
                     if Is_Real then
                        -- More than one found, illegal
                        raise Invalid_Ada with "Numeric literal with "
                          & "multiple decimal points.";
                        
                     elsif In_Exponent then
                        raise Invalid_Ada with
                          "Invalid Numeric literal: '.' is not legal in "
                          & "an exponent.";
                        
                     else
                        if (Inside_Hash 
                              and then not Is_Extended_Digit 
                                (Buffer(I - 1)))
                          or else (not Inside_Hash 
                                     and then not Is_Digit (Buffer(I - 1)))
                        then
                           raise Invalid_Ada with
                             "Numeric literal invalid: Decimal point shall "
                             & "be preceeded with a digit.";
                        end if;
                        
                        Need_More  := True;
                        Is_Real    := True;
                        Decimal_At := I;
                     end if;
                     
                  when '#' =>
                     if Inside_Hash then
                        Last_Hash := I;
                        Inside_Hash := False;
                        
                        if not Is_Extended_Digit (Buffer(I - 1)) then
                           -- Something like: "16##", "16#2.#", "16#2_#,
                           -- all illegal
                           raise Invalid_Ada with
                             "Based numeric literal with invalid based "
                             & "value.";
                        end if;
                        
                     elsif Is_Based then
                        -- Not in a hash, yet based, that means we
                        -- passed two '#' already
                        raise Invalid_Ada with
                          "Numeric literal with > 2 '#' characters";
                        
                     elsif In_Exponent then
                        -- We're in an exponent (after an 'e'), so this
                        -- doesn't work!
                        raise Invalid_Ada with
                          "Invalid Numeric literal: '#' not allowed in "
                          & "an exponent";
                        
                     else
                        -- Not inside hash, and not based, and not in an
                        -- exponent.
                        if Buffer(I - 1) = '.' then
                           raise Invalid_Ada with
                             "Numeric literal invalid: "
                             & "Decimal point must be followed by a digit.";
                        end if;
                        
                        Is_Based    := True;
                        Inside_Hash := True;
                        First_Hash  := I;
                        
                        Need_More := True;
                     end if;
                     
                  when 'e' =>
                     -- Note if In_Hash, 'e' is not considered special, it
                     -- is an extended digit, so we'd never end up here.
                     if In_Exponent then
                        raise Invalid_Ada with
                          "Invalid Numeric literal: multiple 'e's";
                     end if;
                     
                     if (not Is_Digit (Buffer(I - 1)))
                       and then Buffer(I - 1) /= '#'
                     then
                        raise Invalid_Ada with
                          "Invalid Numeric literal: Exponent shall follow "
                          & "a digit or '#'";
                     end if;
                     
                     -- We also need to do a look-ahead here, because the
                     -- there needs to be exactly a '-', '+' or a digit
                     -- following the e
                     declare
                        T: Wide_Wide_Character;
                     begin
                        -- Peek
                        T := Pop_Source;
                        Push_Source (T);
                        
                        if T not in '-' | '+'
                          and then not Is_Digit (T)
                        then
                           raise Invalid_Ada with
                             "Invalid Numeric literal: Exponment shall "
                             & "be followed with +,-, or a digit.";
                        end if;
                        
                     exception
                        when End_Error =>
                           raise Invalid_Ada with 
                             "Invalid numeric literal: "
                             & "source ended at exponent.";
                     end;
                     
                     In_Exponent   := True;
                     Exponent_Mark := I;
                     Exponent_Sign := I;
                     
                     Need_More := True;
                     
                  when '-' | '+' =>
                     if not In_Exponent then
                        if Inside_Hash then
                           raise Invalid_Ada with
                             "Invalid Numeric literal: based literal not "
                             & "terminated";
                           
                        else
                           -- This should be considered as encountering a
                           -- delimiter, and so the literal should be over
                           Push_Source (Buffer(I));
                           exit;
                        end if;
                        
                     elsif Got_Sign then
                        -- Multiple +/- after an exponent
                        -- If they are paired, that's no good, but
                        -- if a number has is before us, then it is really
                        -- just a delimiter
                        if Is_Digit (Buffer(I - 1)) then
                           Push_Source (Buffer(I));
                           exit;
                           
                        else
                           raise Invalid_Ada with
                             "Invalid Numeric literal: multiple signs "
                             & "following exponent";
                        end if;
                        
                     else
                        -- Last character was 'e', so this is our sign
                        Got_Sign := True;
                        Exponent_Sign := I;
                        
                        Need_More := True;
                        
                     end if;
                     
                  when others =>
                     raise Invalid_Ada with
                       "Invalid character in Numeric literal";
               end case;
            end if;
            
            -- Corner case.. If we have a based literal and we are outside
            -- of the hash, then the next character must be 'e', a space,
            -- or a delimiter. Anything else is illegal.
            
            -- We most of this already, and if we made it this far, with
            -- Inside_Hash false, it means that what follows '#' is a digit
            -- or '_'. That is not legal (according to 2.4.2-2), and that
            -- adjacent numeric literals must be separated by a separator.
            
            if not Inside_Hash and then Is_Based and then not Special then
               raise Invalid_Ada with
                 "Invalid Numeric literal: " &
                 "Based literals must end with an exponent, " &
                 "a delimiter, or a at least one separator space.";
            end if;
            
            Last := I;
         end loop;
         
         -- Check for buffer limit
         if Last = Buffer'Last  then
            raise Invalid_Ada with
              "Numeric literal too long: Literal longer than"
              & Natural'Image (Buffer'Last)
              & " is not required to be accepted by the Ada standard";
         end if;
         
         -- Completion checks
         if Inside_Hash                                 -- e.g "16#ABC"
           or else (Is_Real and then Decimal_At = Last) -- e.g ".123"
           or else (In_Exponent and then Exponent_Mark = Last) 
           -- e.g "123e+"
         then
            raise Invalid_Ada with "Incomplete numeric literal";
         end if;
         
         declare
            OK: Boolean := False;
         begin
            if Is_Based then
               OK := Is_Numeral (Buffer(1 .. First_Hash - 1));
               
               if Is_Real then
                  OK := OK 
                    and then 
                    (Is_Based_Numeral 
                       (Buffer(First_Hash + 1 .. Decimal_At - 1))
                       and then
                       Is_Based_Numeral
                         (Buffer(Decimal_At + 1 .. Last_Hash - 1)));
                  
               else
                  OK := OK and then Is_Based_Numeral
                    (Buffer(First_Hash + 1 .. Last_Hash - 1));
               end if;
               
            else
               if Is_Real then
                  OK := Is_Numeral (Buffer(1 .. Decimal_At - 1));
                  if In_Exponent then
                     OK := OK and then Is_Numeral 
                       (Buffer(Decimal_At + 1 .. Exponent_Mark - 1));
                  else
                     OK := OK and then Is_Numeral
                       (Buffer(Decimal_At + 1 .. Last));
                  end if;
                  
               elsif In_Exponent then
                  OK := Is_Numeral (Buffer(1 .. Exponent_Mark - 1));
                  
               else
                  OK := Is_Numeral (Buffer(1 .. Last));
               end if;
            end if;
            
            if In_Exponent then
               OK := OK and then Is_Numeral 
                 (Buffer(Exponent_Sign + 1 .. Last));
            end if;
            
            
            -- Tally it up
            if not OK then
               raise Invalid_Ada with "Numeric literal invalid: "
                 & "bad numerals";
            end if;
         end;
         
      end;
      
      -- We have a valid Numeric literal!
      Skip_Trailing_Separators;
      return (Category => Numeric_Literal,
              Content  => To_Unbounded_WWS (Buffer(1 .. Last)));
   end if;
   
   -- Getting here means: we better see a string literal!
   if Buffer(1) /= '"' then
      raise Invalid_Ada with
        "Not a legal Ada lexical element.";
   end if;
   
   -- The rules (RM 2.6) are fairly simple We should see a sequence of
   -- zero or more graphic characters, followed by a '"'. If we see two
   -- consecutive quotation marks, that counts as a single quotation mark
   -- in the string literal itself. Anything else is just illegal.
   Last := 1;
   for I in Buffer'Range loop
      Buffer(I) := Pop_Source;
      
      if not Is_Graphic_Character (Buffer(I)) then
         raise Invalid_Ada with
           "Illegal characters in String Literal";
      
      elsif Buffer(I) = '"' then
         declare
            Look_Ahead: Wide_Wide_Character := Pop_Source;
         begin
            
            if Look_Ahead /= '"' then
               -- Not a double quotation mark, this means we have the
               -- actual end of the string literal
               Push_Source (Look_Ahead);
               exit;
            end if;
            
            -- Otherwise, it is a double quotation mark in the string
            -- sequence which collapses into an actual quotation mark in the
            -- literal. We've bascially done that already with the second
            -- Pop_Source, which effectively discards the "extra" quotation
            -- mark
         end;
      end if;
      
      Last := I;
   end loop;
   
   -- Check for buffer limit
   if Last = Buffer'Last then
      raise Invalid_Ada with
        "String literal too long: String literal longer than"
        & Natural'Image (Buffer'Last)
        & " is not required to be accepted by the Ada standard";
   end if;
   
   -- :)
   Skip_Trailing_Separators;
   return (Category => String_Literal,
           Content  => To_Unbounded_WWS (Buffer(1 .. Last)));
   
   
exception
   when Invalid_Ada =>
      -- Note the position of the (last) illegal characrer in Last_Post
      Source.Last_Pos := Current_Position;
      raise;
      
end Next_Element;
