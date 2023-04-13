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

separate (Repositories.AURA_Spec_Handling.Check_AURA_Spec)

procedure Check_Package_Declaration is

begin
   -- The AURA Spec is very specific and simple. We are strict about
   -- the checking we do
   
   Next_Element;
   Check (Category = Reserved_Word and then Content = "package",
          "AURA spec shall be a package spec without "
            &        "with clauses");
   
   if not Correct then return; end if;
   
   Next_Element;
   Check (Category = Identifier and then Content = "aura",
          "AURA package spec file does not have correct package name.");
   
   if not Correct then return; end if;

   declare
      No_Pure_Msg: constant String
        := "AURA package shall be declared Pure with an aspect_specification";
   begin
      Next_Element;
      Check (Category = Reserved_Word and then Content = "with", No_Pure_Msg);
      
      if not Correct then return; end if;
      
      Next_Element;
      Check (Category = Identifier and then Content = "pure", No_Pure_Msg);
   end;
   
   if not Correct then return; end if;
   
   Next_Element;
   Check (Category = Reserved_Word and then Content = "is",
          "Malformed package declaration");
   
end Check_Package_Declaration;
