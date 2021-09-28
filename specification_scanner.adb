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

with Ada.Strings.Fixed;
with Ada.Assertions;

with Registrar.Source_Files;

package body Specification_Scanner is
   
   -------------------
   -- Parse_Toolkit --
   -------------------
   
   generic
      Buffer: in out Ada_Lexical_Parser.Source_Buffer;
   package Parse_Toolkit is
      use Ada_Lexical_Parser;
      
      Current_Element: Lexical_Element;
      
      Category: Lexical_Category renames Current_Element.Category;
      
      function Content return Wide_Wide_String is
        (WWU.To_Wide_Wide_String (Current_Element.Content));
      
      procedure Next_Element;
      -- Advance to next element, skipping comments, and updating
      -- Current_Element
      
      function Current_Position return String;
      -- Return a string of format "Line:Col"
      
      procedure Assert_Syntax (Check: Boolean);
      -- Executes Ada.Assertions.Assert with the message :
      -- "Line:Col: Syntax Error. 
      
      procedure Load_Identifier (Name: out WWU.Unbounded_Wide_Wide_String);
      procedure Skip_To_Semicolon;
      
   end Parse_Toolkit;
   
   --------------------------------------------------
   package body Parse_Toolkit is
      
      procedure Next_Element is
      begin
         loop
            Current_Element := Next_Element (Buffer);
            exit when Category /= Comment;
         end loop;
      end Next_Element;
      
      
      function Current_Position return String is
         use Ada.Strings;
         use Ada.Strings.Fixed;
         
         Pos: constant Source_Position := Last_Position (Buffer);
      begin
         return 
           Trim (Source => Positive'Image (Pos.Line),
                 Side   => Both)
           & ':'
           & Trim (Source => Positive'Image (Pos.Column),
                   Side   => Both);
      end Current_Position;
      
      
      procedure Assert_Syntax (Check: Boolean) is
      begin
         Ada.Assertions.Assert
           (Check   => Check,
            Message => Current_Position & ": Syntax error");
      end Assert_Syntax;
      
      
      procedure Load_Identifier (Name: out WWU.Unbounded_Wide_Wide_String) is
         use type WWU.Unbounded_Wide_Wide_String;
      -- Should be called when Current_Element is an Identifier
      begin
         Name := Current_Element.Content;
         -- Copy the Unbounded_Wide_Wide_String directly
         
         loop
            Next_Element;
            
            exit when Category not in Identifier | Delimiter;
            exit when Category = Delimiter and then Content /= ".";
            
            Name := Name & Current_Element.Content;
         end loop;
      end Load_Identifier;
      
      
      procedure Skip_To_Semicolon is
      begin
         Next_Element;
         while not (Category = Delimiter and then Content = ";") loop
            Next_Element;
         end loop;
      end Skip_To_Semicolon;
      
   end Parse_Toolkit;
   
   --
   -- Process Subprograms
   -- 
   
   procedure Scan_Subprogram 
     (Buffer         : in out Ada_Lexical_Parser.Source_Buffer;
      Unit_Tree      : in out Declaration_Trees.Tree;
      Subprogram_Node: in     Declaration_Trees.Cursor);
   
   -- Scan_Subprogram shall be called immediately after encountering
   -- the reserved word "function or procedure". A new node for the
   -- subprogram declaration shall be provided by Subprogram_Node.
   --
   -- Scan_Subprogram will set the name, add child declared entities
   -- for each parameter, the type of function returns, and the expression
   -- of expression functions
   
   
   procedure Scan_Object
     (Buffer   : in out Ada_Lexical_Parser.Source_Buffer;
      Entity   : in out Declared_Entity);
   
   -- Shall be called immediately after an identifier followed by a ':'
   -- delimiter. Node.Name shall be set to the value of the identifier before
   -- calling. Scan_Value_Declaration will set Kind, Expression, Is_Constant,
   -- Is_Renamed, Is_Anon_Access, Subtype_Mark, and Renamed_Entity_Name as
   -- appropriate
   --
   -- This procedure covers:
   -- * object_declaration
   -- * number_declaration
   -- * exception_declaration
   -- * renaming_declaration (for any of the above)
   
   
   procedure Scan_Generic
     (Buffer   : in out Ada_Lexical_Parser.Source_Buffer;
      Unit_Tree: in out Declaration_Trees.Tree;
      Root     : in     Declaration_Trees.Cursor);
   
   -- Shall be called immediate after encountering the reserved word "generic".
   -- Scan_Generic appends generic parameters to the subtree rooted at Root,
   -- and then dispatches to Scan_Specification or Scan_Subprogram as
   -- appropriate, passing Root to the Root/Subprogram_Node parameters.
   --
   -- Is_Generic is set for Root
   
   
   procedure Scan_Specification
     (Buffer   : in out Ada_Lexical_Parser.Source_Buffer;
      Unit_Tree: in out Declaration_Trees.Tree;
      Root     : in     Declaration_Trees.Cursor);
   
   -- Shall be called immediately after encountering the reserved word
   -- package. Scan_Specifcation expects to find the completion of a
   -- "package_specification". The name of the package is used to set the Name
   -- property of the node designated by Root. All subsequent declarations are
   -- assigned as children to the subtree denoted by Root, including recursive
   -- descent into nested packages via recursive calls to Scan_Specification
   
   
   -- Body stubs ------------------------------------
   
   procedure Scan_Subprogram 
     (Buffer         : in out Ada_Lexical_Parser.Source_Buffer;
      Unit_Tree      : in out Declaration_Trees.Tree;
      Subprogram_Node: in     Declaration_Trees.Cursor)
   is separate;
   
   procedure Scan_Object
     (Buffer   : in out Ada_Lexical_Parser.Source_Buffer;
      Entity   : in out Declared_Entity)
   is separate;
   
   procedure Scan_Generic
     (Buffer   : in out Ada_Lexical_Parser.Source_Buffer;
      Unit_Tree: in out Declaration_Trees.Tree;
      Root     : in     Declaration_Trees.Cursor)
   is separate;
   
   procedure Scan_Specification
     (Buffer   : in out Ada_Lexical_Parser.Source_Buffer;
      Unit_Tree: in out Declaration_Trees.Tree;
      Root     : in     Declaration_Trees.Cursor)
   is separate;
   
   
   -----------------------
   -- Scan_Package_Spec --
   -----------------------
   
   procedure Scan_Package_Spec
     (Unit     : in     Registrar.Library_Units.Library_Unit;
      Unit_Tree:    out Declaration_Trees.Tree)
   is
      use Registrar.Source_Files;
      use Ada_Lexical_Parser;
      
      Source: aliased Source_Stream
        := Checkout_Read_Stream (Unit.Spec_File);
      
      Buffer: Source_Buffer (Source'Access);
      
      package Toolkit is new Parse_Toolkit (Buffer); use Toolkit;
      
      Unit_Root: Declaration_Trees.Cursor;
      
   begin
      Unit_Tree := Declaration_Trees.Empty_Tree;
      Unit_Root   := Unit_Tree.Root;
      
      Unit_Tree.Prepend_Child 
        (Parent   => Unit_Tree.Root,
         New_Item => Declared_Entity'(Kind => Package_Declaration,
                                      others => <>));
      
      Unit_Root := Declaration_Trees.First_Child (Unit_Tree.Root);
      
      Next_Element;
      
      -- Scan up until package and then kick off Scan_Specification. Note this
      -- handles (ignores) private packages as well
      while not (Category = Reserved_Word and then Content = "package") loop
         if Category = Reserved_Word and then Content = "generic" then
            Unit_Tree(Unit_Root).Is_Generic := True;
         end if;
         
         Next_Element;
      end loop;
      
      -- Recursive decent starts
      Scan_Specification (Buffer    => Buffer,
                          Unit_Tree => Unit_Tree,
                          Root      => Unit_Root);
      
   end Scan_Package_Spec;
   
end Specification_Scanner;
