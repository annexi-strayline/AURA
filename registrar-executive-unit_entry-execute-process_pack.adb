------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2024, ANNEXI-STRAYLINE Inc.                          --
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

separate (Registrar.Executive.Unit_Entry.Execute)

package body Process_Pack is
   
   use Parse_Pack;
   use Ada_Lexical_Parser;
   
   ------------------
   -- Process_With --
   ------------------
   
   -- Parser_Pack is pointing at a "with" reserved word. Our job is to extract
   -- the subsequent library unit names, including the implicitly withed
   -- parent units, if any.
   --
   -- The only legal presentation is a.b.c, d.e.f, ... x.y.z; As in we expect
   -- to find a number of Identifier elements, separated by '.' Delimiters,
   -- for the construction of a full library unit name, or ',' to delimit the
   -- end of a particular name, finally with a ';' to delimit the end of the
   -- with statement. Anything else is illegal.
   --
   -- Note that there are some language-defined or compiler-defined
   -- "subsystems" such as Ada, Interfaces, and GNAT that should not be
   -- included as dependencies. These are stipped when requestiong library
   -- units - see the sibling package Executive.Library_Units_Request
   
   procedure Process_With is
      New_Depend  : Library_Unit;
      
      Name_Pass   : Positive := 1;
      -- Represents the depth of prefixes in the name currently being built-up
   begin
      New_Depend.State := Requested;
      New_Depend.Kind  := Unknown;
      Next_Element;
      
      loop
         case Category is
            when Delimiter =>
               if Content in "." | "," | ";"
                 and then New_Depend.Name.Empty
               then
                  -- We should only be seeing this after an identifier,
                  -- which would have been added to the name of the new
                  -- dependency
                  Abort_Parse;
               end if;
               
               if Content = "." then
                  -- Each implicitly with'ed parent should be included
                  -- in the dependency set. Every-time we get to a period,
                  -- we should add that current name separately, to account
                  -- for that "parent" unit implicitly withed
                  
                  Dependencies.Include (New_Depend);
                  New_Depend.Name.Append (Content);
                  Name_Pass := Name_Pass + 1;
                  
                  -- An identifier _must_ follow
                  Next_Element;
                  if Category /= Identifier then
                     Abort_Parse;
                  end if;
                  
               elsif Content = "," then
                  -- End of a name, so we need to do add it to the set
                  Dependencies.Include (New_Depend);
                  New_Depend.Name.Set_Name ("");
                  Name_Pass := 1;
                  
                  Next_Element;
                  if Category /= Identifier then
                     Abort_Parse;
                  end if;
                  
               elsif Content = ";" then
                  -- Last item to add to the set
                  Dependencies.Include (New_Depend);
                  exit;
                  
               else
                  -- No other delimiter would be acceptible
                  Abort_Parse;
               end if;
               
               
            when Identifier =>
               -- Prohibit any configuration manifest withs. This means
               -- that any pattern of Subsystem.AURA or children is prohibited
               -- by AURA
               
               Assert
                 (Check => (if Name_Pass = 2 then Content /= "aura"),
                  Message => "AURA Configuration manifests may not be "
                    &        "withed. No subsystem may have a child unit "
                    &        "named AURA in an AURA project.");
                               
               
               New_Depend.Name.Append (Content);
               Next_Element;
               
            when others =>
               Abort_Parse;
               
         end case;
         
      end loop;
      
   end Process_With;
   
   ---------------------------
   -- Process_External_With --
   ---------------------------
   
   procedure Process_External_With is
      New_Depend: Library_Unit;
      
   begin
      New_Depend.State := Requested;
      New_Depend.Kind  := External_Unit;
      Next_Element;
      
      -- We expect to see ("abc.c","def.c", ... "xyz.c");
      
      if not (Category = Delimiter and then Content = "(") then
         Abort_Parse;
      end if;
      
      Next_Element;
      
      if Category /= String_Literal then
         Abort_Parse;
      end if;
      
      loop
         -- We only have the file-name at this stage, which gets a '%'
         -- prepended. The actual subsystem
         -- name will get prepended once we know what it is
         -- (via Process_Staged_Externals)
         
         -- We need to check if this name is hypothentically valid (after it
         -- gets the subsystem name prepended in case the actual external
         -- unit withed includes '%', which we don't allow
         
         Assert
           (Check => Unit_Names.Valid_Unit_Name ("standard%" & Content),
            Message => "External unit file names cannot contain '%'.");
         
         New_Depend.Name.Set_Name ('%' & Content);
         Staged_Externals.Include (New_Depend);
         Next_Element;
         
         if Category = Delimiter then
            if Content = "," then
               Next_Element;
               
               if Category /= String_Literal then
                  Abort_Parse;
               end if;
               
               -- And around again
               
            elsif Content = ")" then
               Next_Element;
               
               if not (Category = Delimiter and then Content = ";") then
                  Abort_Parse;
               end if;
               
               -- All done
               exit;
               
            else
               -- Unexpected
               Abort_Parse;
            end if;
               
         else
            Abort_Parse;
         end if;
      end loop;
      
   end Process_External_With;
   
   -------------------------
   -- Process_Declaration --
   -------------------------
   
   procedure Process_Declaration is
      use type Source_Pack.Source_File_Type;
   begin
      -- First check for "separate", in which case we are really
      -- not interested in the name of the separate unit (the subunit), but
      -- rather the parent unit - mainly so that we can associate the correct
      -- subsystem name with any External_With pragmas in this unit. 
      
      -- The main body of execute invokes Process_Delaration when we are at
      -- one of the relevent reserved words
      
      if Content = "separate" then
         New_Unit.Kind := Subunit;
         
         Next_Element;
         if not (Category = Delimiter and then Content = "(") then
            -- syntax error
            Abort_Parse;
         end if;
         
         Next_Element;
         if Category /= Identifier then
            -- Syntax error
            Abort_Parse;
         end if;
         
         -- Identify the complete name
         New_Unit.Name.Set_Name ("");
         
         loop
            New_Unit.Name.Append (Content);
            Next_Element;
            
            if Category /= Delimiter then
               Abort_Parse;
            end if;
            
            if Content = "." then
               New_Unit.Name.Append (Content);
               Next_Element;
               
               -- What follows this must either be a continuation of the
               -- name.
               
               if Category /= Identifier then
                  Abort_Parse;
               end if;
               
            elsif Content = ")" then
               exit;
               
            else
               Abort_Parse;
            end if;
         end loop;
         
         -- That's all we process for this unit, since we are really
         -- dealing with the parent subunit. In the body of execute,
         -- the file access will be assigned as appropriate prior to
         -- submission
         return;
         
      elsif Content = "package" then
         New_Unit.Kind := Package_Unit;
         
      elsif Content in "procedure" | "function" then
         New_Unit.Kind := Subprogram_Unit;
         
      else
         raise Ada.Assertions.Assertion_Error with
           "Expected separate, package, procedure, or function";
      end if;
      
      -- For all non-subunits, we want to get the name of the unit up until
      -- "is"
      
      -- First check and skip the "body" reserved word if we find it
      Next_Element;
      
      if Category = Reserved_Word and then Content = "body" then
         Assert (Check   => not New_Unit.Is_Generic,
                 Message => "Only specifications can be generic.");
         
         Assert (Check   => Source_Pack.Unit_Source_Type = Source_Pack.Ada_Body,
                 Message =>
                   "Specification sources shall not contain bodies.");
         Next_Element;
      end if;
      
      -- Names should always begin with a valid Identifier
      
      if Category /= Identifier then
         Abort_Parse;
      end if;
      
      New_Unit.Name.Set_Name ("");
      
      loop
         
         if Category = Identifier then
            New_Unit.Name.Append (Content);
            Next_Element;
            
            if Category not in Reserved_Word | Delimiter then
               Abort_Parse;
            end if;
            
         elsif Category = Delimiter and then Content = "." then
            New_Unit.Name.Append (Content);
            Next_Element;
            
            if Category /= Identifier then
               Abort_Parse;
            end if;
            
         elsif (Category = Delimiter
                  and then Content in "(" | ";")
           or else (Category = Reserved_Word 
                      and then Content in "is" | "with" | "renames")
         then
            -- Note this does not catch certain incorrect cases such as
            -- package Thing;, but that is really a problem for the compiler
            -- to deal with
            exit;
            
         else
            Abort_Parse;
            
         end if;
      end loop;
      
   end Process_Declaration;
   
   -----------------------------
   -- Filter_Standard_Library --
   -----------------------------
   
   procedure Filter_Standard_Library is
      use Library_Unit_Sets;
      
      Itr: Cursor := Dependencies.First;
      Del: Cursor;
   begin
      while Has_Element (Itr) loop
         Del := Itr;
         Itr := Next (Itr);
         
         if Dependencies(Del).Name.Subsystem_Name.To_String 
           in "ada" | "interfaces" | "system" | "gnat"
         then
            Dependencies.Delete (Del);
         end if;
      end loop;
      
   end Filter_Standard_Library;
   
   ---------------------------------
   -- Process_Parent_Dependencies --
   ---------------------------------
   
   procedure Process_Parent_Dependencies is
      Parent_Name: Unit_Names.Unit_Name 
        := New_Unit.Name.Parent_Name;
      
      Parent_Depend: Library_Unit;
   begin
      Parent_Depend.State := Requested;
      Parent_Depend.Kind  := Unknown;
      
      while not Parent_Name.Empty loop
         Parent_Depend.Name := Parent_Name;
         Dependencies.Include (Parent_Depend);
         Parent_Name := Unit_Names.Unit_Name (Parent_Name.Parent_Name);
      end loop;
   end Process_Parent_Dependencies;
   
   ------------------------------
   -- Process_Staged_Externals --
   ------------------------------
   
   procedure Process_Staged_Externals is
      procedure Append_Subsystem (Position: in Library_Unit_Sets.Cursor) is
      begin
         -- For non-aura units, the external dependency file is expected to be
         -- in the project root directory, and when entered, gets associated
         -- with the root subsystem "standard".
         --
         -- This works well because standard can't be an actual included
         -- subsystem anyways
         
         Staged_Externals(Position).Name.Prepend
           ((if Order.AURA then 
                Wide_Wide_String'(New_Unit.Name.Subsystem_Name.To_String) 
             else
                Wide_Wide_String'("standard")));
      end Append_Subsystem;
   begin
      Staged_Externals.Iterate (Append_Subsystem'Access);
      Dependencies.Union (Staged_Externals);
   end Process_Staged_Externals;
   

   
end Process_Pack;
