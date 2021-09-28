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

with Ada.Directories;

with Registrar.Registration;

separate (Configuration)

procedure Step_4 (Target: in out Subsystem) is
   use Ada.Directories;
   
   SS_Root_Path: constant String := Current_Directory & '/' 
     & Target.Name.To_UTF8_String;
   
   Search: Search_Type;
   Subdir: Directory_Entry_Type;
   
begin
   -- Note that Codepaths will always be empty for the root configuration
   -- unit, since codepaths are explicitly disallowed. It seemed better form to
   -- let it come here anyways, rather than explicitly ending at Step_3 with a
   -- special case, since the outcome is identical, and performance impact very
   -- minimal. In fact, the root config is only explictly processed once, so
   -- having a check in step 3 would be less performant
   
   for Codepath of Target.Configuration.Codepaths loop
      declare
         Full_Path: constant String 
           := SS_Root_Path & '/' & UBS.To_String (Codepath.Value);
         
         Subdir_Name: constant String := Simple_Name (Full_Path);
      begin
         Start_Search (Search => Search,
                       Directory => Containing_Directory (Full_Path),
                       Pattern   => Subdir_Name,
                       Filter => Filter_Type'(Directory => True,
                                              others    => False));
         
         Assert (Check   => More_Entries (Search),
                 Message => "Could not find codepath """ 
                   & UBS.To_String (Codepath.Value)
                   & '"');
         
         Get_Next_Entry (Search          => Search,
                         Directory_Entry => Subdir);
         
         Registrar.Registration.Enter_Directory
           (Directory      => Subdir,
            AURA_Subsystem => Target);
      end;
   end loop;
   
   Complete (Target);
end Step_4;
