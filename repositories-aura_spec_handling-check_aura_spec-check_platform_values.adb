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

with Ada.Characters.Conversions;

separate (Repositories.AURA_Spec_Handling.Check_AURA_Spec)

procedure Check_Platform_Values is
   
   Have_Family, Have_Flavor, Have_Version, Have_Arch: Boolean := False;
   
   function Have_All return Boolean is
     (Have_Family and Have_Flavor and Have_Version and Have_Arch);
   
   package Expected_Values is
      use Ada.Characters.Conversions;
      
      function Family return Wide_Wide_String is
        (To_Wide_Wide_String (Platform_Info.Platform_Family));
      
      function Flavor return Wide_Wide_String is
        (To_Wide_Wide_String (Platform_Info.Platform_Flavor));
      
      function Version return Wide_Wide_String is
        (To_Wide_Wide_String (Platform_Info.Platform_Version));
      
      function Arch return Wide_Wide_String is
        (To_Wide_Wide_String (Platform_Info.Platform_Architecture));
   end Expected_Values;
   
      
   procedure Verify_Value (Name, Expected_Value: Wide_Wide_String) is
      -- Verify that the pattern following Name is a constant
      -- String declaration, with the correct value and style
      
      -- Any failed check here should cause an abort due to bad
      -- syntax
   begin
      Next_Element;
      Check (Test   => Category = Delimiter 
               and then Content = ":",
             Fail_Message => "expected "":"" following identifier """
               & To_String (Name)
               & """");
      
      if not Correct then return; end if;
      
      Next_Element;
      Check (Test   => Category = Reserved_Word 
               and then Content = "constant",
             Fail_Message => """" & To_String (Name)
               & """ must be a constant");
      
      if not Correct then return; end if;
      
      Next_Element;
      Check (Test   => Category = Identifier
               and then Content = "string",
             Fail_Message => """" & To_String (Name)
               & """ must be a String");
      
      if not Correct then return; end if;
      
      Next_Element;
      Check (Test   => Category = Delimiter
               and then Content = ":=",
             Fail_Message => "expected "":="" following ""String"" """
               & To_String (Name)
               & """");
      
      if not Correct then return; end if;
      
      Next_Element;
      Check (Test   => Category = String_Literal,
             Fail_Message => "Expected String literal");
      
      Check (Test   => Content = Expected_Value,
             Fail_Message => "Expected value for """ 
               & To_String (Name) & """ is """
               & To_String (Expected_Value) & """");
      
      if not Correct then return; end if;
      
      Next_Element;
      Check (Test   => Category = Delimiter
               and then Content = ";",
             Fail_Message => "Expressions are not allowed");
   end Verify_Value;
      
begin
   
   -- Next we expect to find the platform infromation, in any order
   
   while Correct and then not Have_All loop
      Next_Element;
      Check (Test => Category = Identifier,
             Fail_Message => "Unexpected """ 
               &        To_String (Content)
               &        """ in AURA package");
      
      if not Correct then
         exit;
      
      elsif Content = "platform_family" then
         Verify_Value (Content, Expected_Values.Family);
         Have_Family := Correct;
         
      elsif Content = "platform_flavor" then
         Verify_Value (Content, Expected_Values.Flavor);
         Have_Flavor := Correct;
         
      elsif Content = "platform_version" then
         Verify_Value (Content, Expected_Values.Version);
         Have_Version := Correct;
         
      elsif Content = "platform_architecture" then
         Verify_Value (Content, Expected_Values.Arch);
         Have_Arch := Correct;
         
      else
         Check (False, "Illegal identifier """
                  & To_String (Content) & """ in AURA spec:");
      end if;
   end loop;
   
end Check_Platform_Values;
