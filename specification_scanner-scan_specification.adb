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

with Ada.Containers.Vectors;

separate (Specification_Scanner)

procedure Scan_Specification
  (Buffer   : in out Ada_Lexical_Parser.Source_Buffer;
   Unit_Tree: in out Declaration_Trees.Tree;
   Root     : in     Declaration_Trees.Cursor)
is
   use Declaration_Trees;
   use Ada_Lexical_Parser;
   package Toolkit is new Parse_Toolkit (Buffer); use Toolkit;
   
begin
   -- This scanner follows the definition of package_specification - ARM 7.1
   
   -- Expecting the name to follow immediately - this is a full unit name,
   -- so also note '.' delimiters
   Next_Element;
   Assert_Syntax (Category = Identifier);
   
   Unit_Tree(Root).Kind := Package_Declaration;
   Load_Identifier (Unit_Tree(Root).Name);
   
   -- Check for (and ignore) an aspect_specification
   -- This means if we see "when", scan until we see "is"
   while not (Category = Reserved_Word and then Content = "is") loop
      Next_Element;
   end loop;
   
   -- Next are any serries of "basic_declarative_item", possibly separated by
   -- the reserved word "private", and ended by "end", optionally followed by
   -- the package name, followed by ";". We will not verify the name in this
   -- scanning.
   
   loop
      -- From a ';' closing a basic_declarative_item, or "is" from the
      -- start of the package.
      Next_Element;
      
      declare
         New_Ent: Declared_Entity;
      begin
         
         case Category is
            when Delimiter 
              | Numeric_Literal 
              | Character_Literal 
              | String_Literal =>
               
               -- None of these make sense at the start of a
               -- basic_declarative_item
               Assert_Syntax (False);
               
            when Identifier =>
               declare
                  package Identifier_Lists is new Ada.Containers.Vectors
                    (Index_Type   => Positive, 
                     Element_Type => WWU.Unbounded_Wide_Wide_String,
                     "="          => WWU."=");
                  use Identifier_Lists;
                  
                  Ident_List: Vector;
               begin
                  -- Create a new node for each identifier in the
                  -- defining_identifier_list, then load the declaration for
                  -- all items in that list
                  
                  loop
                     Load_Identifier (New_Ent.Name);
                     Ident_List.Append (New_Ent.Name);
                     
                     Assert_Syntax (Category = Delimiter 
                                      and then Content in ":" | ",");
                     
                     exit when Content = ":";
                  end loop;
                  
                  Scan_Object (Buffer => Buffer,
                               Entity => New_Ent);
                  
                  -- Copy out one per identifier
                  for Identifier of Ident_List loop
                     New_Ent.Name := Identifier;
                     Unit_Tree.Append_Child (Parent   => Root,
                                             New_Item => New_Ent);
                  end loop;
               end;
               
            when Reserved_Word =>
               if Content = "generic" then
                  Unit_Tree.Append_Child (Parent   => Root,
                                          New_Item => New_Ent);
                  
                  Scan_Generic (Buffer    => Buffer,
                                Unit_Tree => Unit_Tree,
                                Root      => Last_Child (Root));
                  
                  
               elsif Content = "package" then
                  Unit_Tree.Append_Child (Parent   => Root,
                                          New_Item => New_Ent);
                  Scan_Specification 
                    (Buffer    => Buffer,
                     Unit_Tree => Unit_Tree,
                     Root      => Last_Child (Root));
                  
               elsif Content in "overriding" then
                  -- Just skip over this - though procedure/function should be
                  -- next. For scanning, we assume this
                  null;
                  
               elsif Content in "procedure" | "function" then
                  
                  Unit_Tree.Append_Child (Parent   => Root,
                                          New_Item => New_Ent);
                  Scan_Subprogram (Buffer          => Buffer,
                                   Unit_Tree       => Unit_Tree,
                                   Subprogram_Node => Last_Child (Root));
                  
               elsif Content = "type" then
                  New_Ent.Kind := Type_Declaration;
                  
                  Next_Element;
                  Assert_Syntax (Category = Identifier);
                  New_Ent.Name := Current_Element.Content;
                  Skip_To_Semicolon;
                  Unit_Tree.Append_Child (Parent   => Root,
                                          New_Item => New_Ent);
                  
               elsif Content = "subtype" then
                  New_Ent.Kind := Subtype_Declaration;
                  
                  Next_Element;
                  Assert_Syntax (Category = Identifier);
                  New_Ent.Name := Current_Element.Content;
                  Assert_Syntax (Category = Reserved_Word 
                                   and then Content = "is");
                  Next_Element;
                  Assert_Syntax (Category = Identifier);
                  Load_Identifier (New_Ent.Subtype_Mark);
                  Skip_To_Semicolon;
                  Unit_Tree.Append_Child (Parent   => Root,
                                          New_Item => New_Ent);
                  
               elsif Content in "pragma" | "for" | "use" then
                  -- pragmas, aspect_clauses, use_clauses
                  Skip_To_Semicolon;
                  
               elsif Content in "private" then
                  -- This is a declaration starting with private, which
                  -- in the context of a package specification, can only
                  -- be the private part. We simply skip these
                  return;
                  
               elsif Content = "end" then
                  -- We won't verify the optional identifier here, if any,
                  -- again that's up to the compiler, we're just scanning!
                  Skip_To_Semicolon;
                  return;
                  
               else
                  -- No other Reserved_Word 
                  Assert_Syntax (False);
               end if;
               
            when Comment =>
               -- This won't actually happen since Next_Element skips comments.
               -- Even if it did, null is the appropriate action
               null;
               
         end case;
      end;
   end loop;
end Scan_Specification;
