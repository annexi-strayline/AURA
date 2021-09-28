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

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Wide_Wide_Unbounded;

with Ada_Lexical_Parser;     use Ada_Lexical_Parser;
with Registrar.Source_Files;

separate (Build.Recompilation_Check_Orders)

procedure Test_Isolated (Unit    : in out Registrar.Library_Units.Library_Unit;
                         Isolated:    out Boolean)
is 
   package WWU renames Ada.Strings.Wide_Wide_Unbounded;
   End_Error: exception renames Ada.IO_Exceptions.End_Error;
   
   Spec_Stream: aliased Registrar.Source_Files.Source_Stream
     := Registrar.Source_Files.Checkout_Read_Stream (Unit.Spec_File);
   
   Source: Source_Buffer (Spec_Stream'Access);
   E: Lexical_Element;
   
   function Content return Wide_Wide_String 
     is (WWU.To_Wide_Wide_String (E.Content));
   
   function Category return Lexical_Category is (E.Category);
   
   procedure Next_Element is
   begin
      loop
         -- Also skip comments
         E := Next_Element (Source);
         exit when Category /= Comment;
      end loop;
   end Next_Element;
   
   procedure Recursive_Skip_Parens with
     Pre  => Category = Delimiter and then Content = "(",
     Post => Category = Delimiter and then Content = ")";
   
   procedure Recursive_Skip_Parens is
   begin
      Next_Element;
      
      while not (Category = Delimiter and then Content = ")") loop
         if Category = Delimiter and then Content = "(" then
            Recursive_Skip_Parens;
         end if;
         
         Next_Element;
      end loop;
   end Recursive_Skip_Parens;
   
begin
   
   Next_Element;
   
   loop
      if Category = Reserved_Word then
         if Content = "generic" then
            -- That's as much as we need to know!
            Isolated := False;
            return;
            
         elsif Content = "pragma" then
            Next_Element;
            if Category = Identifier and then
              Content in "inline" | "inline_always"  -- inline_always is
            then                                     -- GNAT-specific
               Isolated := False;
               return;
            end if;
            
         elsif Content = "with" then
            Next_Element;
            
            while not (Category = Delimiter and then Content = ";") loop

               
               if Category = Identifier 
                 and then Content in "inline" | "inline_always" then
                  -- This is admittedly lazy. If there was an expression here
                  -- somewhere that refered to user-declared object with the
                  -- name "inline", then we might wrongly classify this unit
                  -- as not-isolated.
                  
                  -- Dealing with this likely rare case is for a later time
                  Isolated := False;
                  return;
                  
               elsif Category = Delimiter and then Content = "(" then
                  -- Expressions need to be totally skipped
                  Recursive_Skip_Parens;
                  
               end if;
               
               Next_Element;
               
               if Category = Delimiter and then Content = "=>" then
                  -- The next item could be an actual user-defined identifier,
                  -- which could (legally) be "inline". Skip to the next ',',
                  -- ';' or '('.
                  Next_Element;
                  
                  while
                    not (Category = Delimiter 
                           and then Content in "," | ";" | "(") 
                  loop
                     Next_Element;
                  end loop;

               end if;
               
            end loop;
            
         else
            Next_Element;
         end if;
         
      else
         Next_Element;
      end if;
   end loop;
   
   -- Unreachable
   pragma Assert (False);
   
exception
   
   when End_Error =>
      Isolated := True;
      -- This means we got the end of the file without finding anything that
      -- told us this wasn't isolated
      
   when Invalid_Ada =>
      Isolated := False;
      -- Let's not go beyond our jurisdiction. This is really not the place
      -- to be showing errors regarding the unit sources. We'll just assume
      -- the worst here and let the compiler tell the user what is wrong
      
   when others => raise;
      
end Test_Isolated;
