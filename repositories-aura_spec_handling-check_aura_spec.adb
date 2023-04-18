------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2023, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Handling;
with Ada.Characters.Conversions;


with User_Notices;
with Ada_Lexical_Parser; 


separate (Repositories.AURA_Spec_Handling)

procedure Check_AURA_Spec 
  (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
   Correct : out Boolean)
is
   use Ada_Lexical_Parser;
   
   End_Error: exception renames Ada.IO_Exceptions.End_Error;
   
   Source: Source_Buffer (Stream);
   E: Lexical_Element;
   
   function To_String (Item      : in Wide_Wide_String;
                       Substitute: in Character := ' ')
                      return String
     renames Ada.Characters.Conversions.To_String;
   
   procedure Next_Element is
   begin
      loop
         E := Next_Element (Source);
         
         exit when E.Category /= Comment;
      end loop;
   end Next_Element;
   
   function Category return Lexical_Category is (E.Category);
   function Content  return Wide_Wide_String is 
     (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (E.Content));
   
   
   Notices: User_Notices.Notice_Lines;
   
   procedure Check (Test: in Boolean; Fail_Message: in String) is
      use Ada.Strings.Unbounded;
      Position: constant Source_Position := Last_Position (Source);
      
      function Trim (Source: in String;
                     Side  : in Ada.Strings.Trim_End := Ada.Strings.Both)
                    return String
        renames Ada.Strings.Fixed.Trim;
   begin
      if Test then return; end if;
      
      -- We generally expect the common case being for all checks to
      -- pass. Therefore we can afford to lazily install the notice header
      
      if Correct then
         -- No tests have failed yet
         Notices.Append
           (To_Unbounded_String
              ("AURA specification is currently invalid:"));
         Correct := False;
      end if;
      
      Notices.Append 
        (To_Unbounded_String 
           ('(' 
              & Trim(Positive'Image(Position.Line)) & ':'
              & Trim(Positive'Image(Position.Column)) & "): "
              & Fail_Message));
   end Check;
   
   procedure Check_Package_Declaration is separate;
   procedure Check_Repository_Formats  is separate;
   procedure Check_Platform_Values     is separate;
   procedure Check_Package_Completion  is separate;
   
   Checklist: constant array (1 .. 4) of not null access procedure
     := (1 => Check_Package_Declaration'Access,
         2 => Check_Repository_Formats'Access,
         3 => Check_Platform_Values'Access,
         4 => Check_Package_Completion'Access);
begin
   
   Correct := True;
   -- All following checks are assertive.

   for Item of Checklist loop
      Item.all;
      exit when not Correct;
   end loop;
   
   if not Correct then
      User_Notices.Post_Notice (Notices);
   end if;
   
exception
   when End_Error =>
      Check (False, "Unexpected end of file.");
      
   when Invalid_Ada =>
      Check (False, "Parser: Invalid Ada.");
      
end Check_AURA_Spec;
