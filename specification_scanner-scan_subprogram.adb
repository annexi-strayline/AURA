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

separate (Specification_Scanner)

procedure Scan_Subprogram 
     (Buffer         : in out Ada_Lexical_Parser.Source_Buffer;
      Unit_Tree      : in out Declaration_Trees.Tree;
      Subprogram_Node: in     Declaration_Trees.Cursor)
is
   use Ada_Lexical_Parser;
   use Declaration_Trees;
   package Toolkit is new Parse_Toolkit (Buffer); use Toolkit;
   
   use type WWU.Unbounded_Wide_Wide_String;
   
   This_Ent: Declared_Entity renames Unit_Tree(Subprogram_Node);
   
   procedure Add_Parameter is
      Parameter_Ent: Declared_Entity;
   begin
      Parameter_Ent.Kind := Object_Declaration;
      Parameter_Ent.Name := Current_Element.Content;
      Next_Element;
      Assert_Syntax (Category = Delimiter and then Content = ":");
      Next_Element;
      
      while Category = Reserved_Word loop
         Assert_Syntax (Content in 
                            "in"  | "out" 
                          | "not" | "null" | "access" 
                          | "aliased");
         Next_Element;
      end loop;
      
      Assert_Syntax (Category = Identifier);
      Load_Identifier (This_Ent.Subtype_Mark);
   end Add_Parameter;
   
begin
   This_Ent.Kind := Subprogram_Declaration;
   Load_Identifier (This_Ent.Name);
   
   Assert_Syntax (Category in Delimiter | Reserved_Word
                    and then Content in 
                       ";" | "("    | "return" 
                    | "is" | "with" | "renames");
   
   if Category = Delimiter and then Content = "(" then
      -- Process parameters
      loop
         Add_Parameter;
         Assert_Syntax (Category = Delimiter);
         
         exit when Content = ")";
         
         Assert_Syntax (Content = ";");
      end loop;
   
      Next_Element;
   end if;
   
   if Category = Delimiter and then Content = ";" then
      return;
   end if;
   
   Assert_Syntax (Category in Reserved_Word
                    and then Content in "return" | "is" | "with" | "renames");
   
   if Content = "return" then
      -- Record the subtype mark
      Next_Element;
      Assert_Syntax (Category = Identifier);
      Load_Identifier (This_Ent.Subtype_Mark);
   end if;
   
   if Category = Identifier and then Content = ";" then
      return;
   end if;
   
   Assert_Syntax (Category = Reserved_Word
                    and then Content in "is" | "with" | "renames");
   
   if Content = "is" then
      -- Either a function expression or a null procedure. We allow either
      -- without checking if it really is a function or a procedure because
      -- this is just a scanner. We care about an "is" only because we want
      -- to extract the expression
      Next_Element;
      
      Assert_Syntax 
        ((Category = Delimiter and then Content = "(")
           or else (Category = Reserved_Word and then Content = "null"));
      

      
      if Content = "(" then
         -- For ease of parsing, we will pull in the expression including
         -- the surrouding parenthesis. This allivates us from needing to
         -- track depth. We simply go until we hit "with" or ";".
         
         This_Ent.Expression := Current_Element.Content;
         
         loop
            Next_Element;
            exit when Category = Delimiter and then Content = ";";
            exit when Category = Reserved_Word and then Content = "with";
            
            This_Ent.Expression 
              := This_Ent.Expression & Current_Element.Content;
         end loop;
         
      elsif Content = "null" then
         Next_Element;
         Assert_Syntax (Category = Delimiter and then Content = ";");
         return;
         
      end if;
   end if;
   
   if Category = Identifier and then Content = ";" then
      return;
   end if;
   
   Assert_Syntax (Category = Reserved_Word
                    and then Content in "with" | "renames");
   -- For renames, rename must come before "with"
   -- Also, expression functions obviously cannot also be a rename
   
   if Content = "renames" then
      Assert_Syntax (WWU.Length (This_Ent.Expression) = 0);
      
      This_Ent.Is_Renaming := True;
      Next_Element;
      Assert_Syntax (Category = Identifier);
      Load_Identifier (This_Ent.Renamed_Entity_Name);
   end if;
   
   Assert_Syntax (Category = Reserved_Word and then Content = "with");
   -- We're not interested in the aspect_specification part
   Skip_To_Semicolon;
   
end Scan_Subprogram;
