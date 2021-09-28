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

with Ada.Strings.Wide_Wide_Maps;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Unicode.General_Category;
with Unicode.Case_Folding.Simple;

package body Unit_Names is
   
   package UTF8 renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   
   Name_Delimiter_Set: constant 
     Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set 
     := Ada.Strings.Wide_Wide_Maps.To_Set (".%");
   
   ---------------
   -- Case_Fold --
   ---------------
   
   procedure Case_Fold (Buffer: in out Wide_Wide_String) is
   begin
      for C of Buffer loop
         C := Unicode.Case_Folding.Simple (C);
      end loop;
   end Case_Fold;
   
   
   ---------
   -- "<" --
   ---------
   
   function "<" (Left, Right: Unit_Name) return Boolean is
     (WWU."<" (Left.Name_String, Right.Name_String));
   
   -------------------
   -- Match_Initial --
   -------------------
   
   function  Match_Initial (Name: Unit_Name; Initial: Wide_Wide_String)
                           return Boolean
   is
      use Ada.Strings, WWU;
      
      Initial_Folded: Wide_Wide_String := Initial;
   begin
      Case_Fold (Initial_Folded);
      
      return 
        Index (Source  => Name.Name_String,
               Pattern => Initial_Folded)   = 1;
      
      
   end Match_Initial;
   
   ---------------------
   -- Valid_Unit_Name --
   ---------------------
   
   function Valid_Unit_Name (Candidate: in Wide_Wide_String) return Boolean is
      use Unicode.General_Category;
      use Ada.Strings.Wide_Wide_Fixed;
      
      function Valid_Operator_Symbol (Slice: in Wide_Wide_String)
                                     return Boolean
      is
         Folded: Wide_Wide_String := Slice;
      begin
         -- RM 6.1-9,10/3
         -- operator_symbol ::= string_literal
         --
         -- The sequence of characters in an operator_symbol shall form:
         -- - a reserved word
         -- - a delimiter/compound delimiter
         -- 
         -- And corresponds to an operator belonging ot one of the six
         -- categories of operators defined in subclause 4.5, which means:
         --
         -- "=", "/=", "<", "<=", ">", ">=", "+", "-", "&", "*", "**", "/"
         -- "mod", "rem", "abs", "not", "and", "or", "xor"
         
         -- Must be a string literal
         if not (Slice(Slice'First) = '"' 
                   and then Slice(Slice'Last) = '"') 
         then
            return False;
         end if;
         
         
         Case_Fold (Folded);
         return Folded in 
           "="   | "/=" | "<"  | "<=" | ">"   | ">="  | "+"   | "-"   |
           "&"   | "*"  | "**" | "/"  | "mod" | "rem" | "abs" | "not" | 
           "and" | "or" | "xor";
         
      end Valid_Operator_Symbol;
      
      
      function Valid_Identifier (Slice: in Wide_Wide_String) return Boolean is
         function Valid_Start (C: Wide_Wide_Character) return Boolean is
           (General_Category (C) in 
              Letter_Uppercase | Letter_Lowercase | Letter_Titlecase |
              Letter_Modifier  | Letter_Other     | Number_Letter);
         
         function Valid_Extend (Last, This: Wide_Wide_Character)
                               return Boolean 
         is
            Last_Cat: constant General_Category_Type := General_Category (Last);
            This_Cat: constant General_Category_Type := General_Category (This);
         begin
            if Valid_Start (This) then 
               -- RM 2.3-2/2
               return True;
               
            elsif Last_Cat = Punctuation_Connector
              and then This_Cat = Punctuation_Connector
            then
               -- Two consecutive Punctuation_Connectors Not allowed: RM 2.3-4/3
               return False;
               
            else
               -- RM 2.3-3.1/3
               return This_Cat in 
                 Mark_Nonspacing      | Mark_Spacing_Combining | 
                 Number_Decimal_Digit | Punctuation_Connector;
            end if;
         end Valid_Extend;
         
         function Valid_Final (Last, This: Wide_Wide_Character) 
                              return Boolean is 
           (Valid_Extend (Last, This) and then
              -- RM 2.3-4/3
              General_Category (This) /= Punctuation_Connector);
      begin
         if Slice'Length = 0 
           or else not Valid_Start (Slice(Slice'First)) 
         then
            return False;
         end if;
         
         for I in Slice'First + 1 .. Slice'Last loop
            if not Valid_Extend (Last => Slice(I - 1),
                                 This => Slice(I))
            then
               return False;
            end if;
         end loop;
         
         if Slice'Length > 1 
           and then not Valid_Final (Last => Slice(Slice'Last - 1),
                                     This => Slice(Slice'Last))
         then
            return False;
            
         else
            return True;
         end if;
         
      end Valid_Identifier;
      
      
      type Slice_Bounds is
         record
            First, Last: Natural;
         end record;
      
      Target_Slice: Slice_Bounds;
      
   begin
      -- Quick sanity checks
      if Candidate'Length = 0 then
         return True;
      end if;
      
      if Count (Source  => Candidate,
                Pattern => "%") > 1
      then
         -- These are the special AURA-specific "external unit" names, which
         -- only permit a single '%' appearing
         return False;
         
      end if;
      
      -- The name should never start with a Name_Delimiter. 
      
      if Index (Source => Candidate,
                Set    => Name_Delimiter_Set,
                From   => Candidate'First)     = Candidate'First
      then
         return False;
      end if;
      

         
      -- First component should always be either an identifier, or an operator
      -- symbol
      
      Target_Slice := (First => Candidate'First,
                       Last  => Candidate'First - 1);
      
      while Target_Slice.First < Candidate'Last loop
         
         if Candidate(Target_Slice.First) = '"' then
            -- Operator symbols should either be the only thing in the name,
            -- or they should be the last thing in the name
            
            Target_Slice.First := Candidate'First;
            Target_Slice.Last  := Index (Source  => Candidate,
                                         Pattern => """",
                                         From    => Candidate'First + 1);
            
            if Target_Slice.Last = Candidate'Last
              and then Valid_Operator_Symbol 
                (Candidate(Target_Slice.First .. Target_Slice.Last))
            then
               return True;
               
            else
               return False;
            end if;
            
         else
            -- Must be an identifier. Set the end of the slice to be the next
            -- name delimiter (if any)
            
            Target_Slice.Last := Index (Source => Candidate,
                                        Set    => Name_Delimiter_Set,
                                        From   => Target_Slice.First);
            
            if Target_Slice.Last = 0 then
               -- Indentifier should go right to the end
               Target_Slice.Last := Candidate'Last;
               
            else
               -- Adjust the slice backwards one, since we don't want to
               -- include the delimiter
               Target_Slice.Last := Target_Slice.Last - 1;
               
            end if;
            
            if Valid_Identifier 
              (Candidate(Target_Slice.First .. Target_Slice.Last))
            then
               -- Looks good. If the delimiter that follows is '%', then
               -- the rest is just a file name. Also we'll take the moment
               -- to ensure there are no '.' before the '%', since that is not
               -- legal for AURA external unit names, which are per-subsystem
               
               if Target_Slice.Last < Candidate'Last 
                 and then Candidate(Target_Slice.Last + 1) = '%'
               then
                  return Count 
                    (Source  => Candidate(Candidate'First .. Target_Slice.Last),
                     Pattern => ".") = 0;
                  
               else
                  -- Set up next round by skipping the last delimiter.
                  -- If there is no last delimiter, we'll be beyond the bounds
                  -- of Candidate, and the loop will terminate
                  Target_Slice.First := Target_Slice.Last + 2;
                  
               end if;
               
               
            else
               return False;
               
            end if;
         end if;
      end loop;
      
      -- Making it out of the loop implies we found nothing wrong.
      return True;
      
   exception
      when others => return False;
      
   end Valid_Unit_Name;
   
   ----------------------
   -- Is_External_Unit --
   ----------------------
   
   function  Is_External_Unit (Name: Unit_Name) return Boolean is
   begin
      return WWU.Count (Source  => Name.Name_String,
                        Pattern => "%")              > 0;
   end Is_External_Unit;
   
   
   ---------------
   -- To_String --
   ---------------
   
   function To_String (Name: Unit_Name) return Wide_Wide_String
     is (WWU.To_Wide_Wide_String (Name.Name_String));
   
   --------------------
   -- To_UTF8_String --
   --------------------
   
   function To_UTF8_String (Name: Unit_Name) return String
     is (UTF8.Encode (Name.To_String));
   
   
   --------------
   -- Set_Name --
   --------------
   
   procedure Set_Name (Name:    out Unit_Name;
                       S   : in     Wide_Wide_String)
   is 
      Folded: Wide_Wide_String := S;
   begin
      Case_Fold (Folded);
      
      WWU.Set_Unbounded_Wide_Wide_String 
        (Target => Name.Name_String,
         Source => Folded);
   end Set_Name;
   
   
   function Set_Name (S: Wide_Wide_String) return Unit_Name is
   begin
      return Name: Unit_Name do
         Name.Set_Name (S);
      end return;
   end Set_Name;
   
   -------------
   -- Prepend --
   -------------
   
   procedure Prepend (Name: in out Unit_Name; S: in Wide_Wide_String) is
      New_Content: Unit_Name := Set_Name (S);
      -- Take advantage of the existing case folding of Set_Name to ensure
      -- that the appended string is also folded. Though this is often
      -- not necessary, it is consistent, and thus probably expected.
      
      use WWU;
   begin
      Name.Name_String := New_Content.Name_String & Name.Name_String; 
   end Prepend;
   
   ------------
   -- Append --
   ------------
   
   procedure Append (Name: in out Unit_Name; S: in Wide_Wide_String) is
      New_Content: Unit_Name := Set_Name (S);
      -- Take advantage of the existing case folding of Set_Name to ensure
      -- that the appended string is also folded. Though this is often
      -- not necessary, it is consistent, and thus probably expected.
      
   begin
      WWU.Append (Source   => Name.Name_String,
                  New_Item => New_Content.Name_String);
   end Append;
   
   ---------
   -- "&" --
   ---------
   
   function "&" (Left: Unit_Name; Right: Wide_Wide_String) return Unit_Name is
   begin
      return Concat: Unit_Name := Left do
         Concat.Append (Right);
      end return;
   end "&";
   
   --------------------------------------------------
   function "&" (Left: Wide_Wide_String; Right: Unit_Name) return Unit_Name is
   begin
      return Concat: Unit_Name := Right do
         Concat.Prepend (Left);
      end return;
   end "&";
   
   --------------------------------------------------
   function "&" (Left, Right: Unit_Name) return Unit_Name is
     ((Name_String => WWU."&" (Left.Name_String, Right.Name_String)));
      
   
   -----------
   -- Empty --
   -----------
   
   function Empty (Name: Unit_Name) return Boolean 
     is (WWU.Length (Name.Name_String) = 0);
   
   
   -----------------
   -- Parent_Name --
   -----------------
   
   function Parent_Name (Name: Unit_Name) return Unit_Name is 
      use Ada.Strings;
      use WWU;
      
      Up_Dot: Natural;
   begin
      -- Parent_Name should not be invoked on External_Units
      pragma Assert (Count (Source  => Name.Name_String,
                            Pattern => "%") 
                       = 0);
      
      Up_Dot := Index (Source  => Name.Name_String,
                       Pattern => ".",
                       Going   => Backward);
      
      -- Return a slice of everything preceeding the dot, exclusive
      
      return Unit_Name
        '(Name_String => Unbounded_Slice 
            (Source => Name.Name_String,
             Low    => 1,
             High   => (if Up_Dot = 0 then 0 else Up_Dot - 1)));
   end Parent_Name;
   
   --------------------
   -- Subsystem_Name --
   --------------------
   
   function Subsystem_Name (Name: Unit_Name) return Unit_Name is
      Extern_Delimit_Index: constant Natural 
        := WWU.Index (Source  => Name.Name_String,
                      Pattern => "%",
                      From    => 1);
      
      Prefix_Index: constant Natural := WWU.Index (Source  => Name.Name_String,
                                                   Pattern => ".",
                                                   From    => 1);
   begin
      if Prefix_Index = 0 and then Extern_Delimit_Index = 0 then
         -- Not a prefixed name, or external unit. Assume this is a "top-level"
         -- package name (subsystem)
         return Name;
         
      elsif Extern_Delimit_Index > 0 then
         return Subsystem:Unit_Name do
            WWU.Unbounded_Slice (Source => Name.Name_String,
                                 Target => Subsystem.Name_String,
                                 Low    => 1,
                                 High   => Extern_Delimit_Index - 1);
         end return;
         
      else
         return Subsystem: Unit_Name do
            WWU.Unbounded_Slice (Source => Name.Name_String,
                                 Target => Subsystem.Name_String,
                                 Low    => 1,
                                 High   => Prefix_Index - 1);
         end return;
      end if;
   end Subsystem_Name;
   
   ----------------------
   -- Self_Direct_Name --
   ----------------------
   
   function Self_Direct_Name (Name: Unit_Name) return Unit_Name is
      use Ada.Strings;
      
      Last_Index: constant Natural := WWU.Length (Name.Name_String);
      
      Last_Prefix: constant Natural := WWU.Index (Source => Name.Name_String,
                                                  Set    => Name_Delimiter_Set,
                                                  From   => Last_Index,
                                                  Going  => Backward);
   begin
      if Last_Prefix = 0 then
         return Name;
      else
         return N: Unit_Name do
            WWU.Unbounded_Slice (Source => Name.Name_String,
                                 Target => N.Name_String,
                                 Low    => Last_Prefix + 1,
                                 High   => Last_Index);
         end return;
      end if;
      
   end Self_Direct_Name;
   
end Unit_Names;
