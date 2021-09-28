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

procedure Scan_Object
  (Buffer   : in out Ada_Lexical_Parser.Source_Buffer;
   Entity   : in out Declared_Entity)
is
   use Ada_Lexical_Parser;
   use Declaration_Trees;
   package Toolkit is new Parse_Toolkit (Buffer); use Toolkit;
   
   procedure Load_Expression is
      -- Should be called after encountering ":="
      
      use WWU;
   begin
      -- Fairly simple really, just loop until we see a semicolon or "with"
      -- (indicating an aspect_specification)
      Entity.Expression := Null_Unbounded_Wide_Wide_String;
      
      loop
         Next_Element;
         
         exit when Category = Delimiter and then Content = ";";
         exit when Category = Reserved_Word and then Content = "with";
         
         Entity.Expression := Entity.Expression & Current_Element.Content;
      end loop;
   end Load_Expression;
   
begin
   -- So following an "Identifier:" pattern, we are expecting to
   -- see one of three posibilities:
   -- 1. A subtype mark (object_declaration)     (possibly constant)
   -- 2. A numeric literal (number_declaration)  (necessarily consant)
   -- 3. "exception" (exception_declaration)
   
   -- Therefore what follows the ":" must be either an Identifier,
   -- or a Reserved_Word (constant/renames/not (null)/access)
   
   Next_Element;
   
   -- Ignore an aliased
   if Category = Reserved_Word and then Content = "aliased" then
      Next_Element;
   end if;
   
   -- We cannot have both constant and renames
   
   if Category = Reserved_Word then
      if Content = "constant" then
         Entity.Is_Constant := True;
         Next_Element;
         
      elsif Content = "not" then
         Next_Element;
         Assert_Syntax (Category = Reserved_Word and then Content = "null");
      end if;
   end if;
   
   -- Catch access_defintions or named numbers now. If constant or not
   -- triggered, we have advanced to the next element. That element will be
   -- "access" in the case of an  access_definition, or ':=' in the case
   -- of a named number (and Is_Constant must be true)
   
   if Category = Reserved_Word and then Content = "access" then
      Next_Element;
      
      if Category = Reserved_Word then
         if Content = "constant" then
            Assert_Syntax (not Entity.Is_Constant);
            Entity.Is_Constant := True;
            
         elsif Content = "all" then
            null;
            
         else
            Assert_Syntax (False);
         end if;
         
         Next_Element;
      end if;
      
   elsif Category = Delimiter and then Content = ":=" then
      -- This should be a named number. We won't (can't) go through the work of
      -- actually evaluating the expression or doing name resolution, so we'll
      -- have to assume it is a valid static expression.
      Assert_Syntax (Entity.Is_Constant);
      Load_Expression;
      -- Aspect specifcations are not allowed for named numbers
      Assert_Syntax (Category = Delimiter and then Content = ";");
      Entity.Kind := Number_Declaration;
      return;
      
   else
      -- Object definition
      Entity.Kind := Object_Declaration;
   end if;
   

   -- X: [constant/access/not null [access]] subtype_mark renames/:=
   --                                        ^^^^^^^^^^^^
   --                                        We are here
   Assert_Syntax (Category = Identifier);   
   Load_Identifier (Entity.Subtype_Mark);
   
   if Category = Reserved_Word and then Content = "renames" then
      -- Object renaming
      Next_Element;
      Assert_Syntax (Category = Identifier);
      Entity.Is_Renaming := True;
      Load_Identifier (Entity.Renamed_Entity_Name);
      
   elsif Category = Delimiter and then Content = ":=" then
      -- Object declaration with initialization
      Load_Expression;
      
   elsif Category = Delimiter and then Content = ";" then
      -- Deferred constant or object decl with default init
      null;
      
   elsif Category = Reserved_Word and then Content = "with" then
      -- Deferred constant or object decl with default init and an
      -- aspect_specifcation.
      --
      -- We ignore aspect_specifications
      
      Skip_To_Semicolon;
   else
      Assert_Syntax (False);
   end if;
   
   
end Scan_Object;
