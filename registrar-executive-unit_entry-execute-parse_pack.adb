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

package body Parse_Pack is
   
   -------------
   -- Content --
   -------------
   
   function Content return Wide_Wide_String 
     is (WWU.To_Wide_Wide_String (E.Content));
   
   --------------
   -- Category --
   --------------
   
   function Category return Lexical_Category is (E.Category);
   
   ------------------
   -- Next_Element --
   ------------------
   
   procedure Next_Element is
   begin
      loop
         -- Also skip comments
         E := Next_Element (Source);
         exit when Category /= Comment;
      end loop;
   end Next_Element;
   
   -----------------------
   -- Skip_To_Semicolon --
   -----------------------
   
   procedure Skip_To_Semicolon is
   begin
      -- Keep going until we find a ';' delimiter
      loop
         Next_Element;
         exit when Category = Delimiter 
           and then Content = ";";
      end loop;
   end Skip_To_Semicolon;
   
   -----------------------
   -- File_And_Position --
   -----------------------
   
   function File_And_Position return String is
         Pos: Source_Position := Last_Position (Source);
      begin
         return UBS.To_String (Order.File_Full_Name) & ':'
           & Positive'Image (Pos.Line) & ':'
           & Positive'Image (Pos.Column);
   end File_And_Position;
   
   -----------------
   -- Abort_Parse --
   -----------------
   
   procedure Abort_Parse is
   begin
      raise Invalid_Unit with
        "Unexpected " & Lexical_Category'Image (E.Category) 
        & " element " 
        & '"' 
        & Ada.Characters.Conversions.To_String 
          (WWU.To_Wide_Wide_String (E.Content))
        & '"' & " at "
        & File_And_Position;
   end Abort_Parse;
   
   
end Parse_Pack;
