------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2023, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

with CLI;
with UI_Primitives;
with User_Queries;

with Unit_Names;
with Registrar.Subsystems;
with Registrar.Library_Units;
with Registrar.Queries;

separate (Depreciation_Handlers)
procedure AURA_Subdirectory (OK_To_Proceed: out Boolean) is
   
   AURA_Subsystem_Name: constant Unit_Names.Unit_Name
     := Unit_Names.Set_Name ("aura");
   
   procedure Process_Changes is separate;
   
   Query_Active: Boolean := False;
   
begin
   -- First check to see if we have the AURA subsystem registered, otherwise
   -- we can continue normally.
   --
   -- Since this handler is invoked after Enter_Root, which only enters units
   -- in the project root (and not any subsystem subdirectories), then we
   -- expect to have the aura subsystem registered at this point if any only if
   -- aura subsystem library units exist in the project root.
   
   if not Registrar.Queries.Subsystem_Registered (AURA_Subsystem_Name) then
      OK_To_Proceed := True;
      return;
   else
      OK_To_Proceed := False;
   end if;
   
   -- Get user's go-ahead before proceeding. This is a little awkward since
   -- we will be invoking the User_Queries.Query_Manager to deliver, handle,
   -- and obtain the result of query kind of directly.
   --
   -- Query_Manager is really meant to be invoked from the Worker tasks, but
   -- for the purposes of this handler, we really don't need to make things
   -- that complicated
   --
   -- Since this handler is called between parallelized opereations, we
   -- shouldn't expect there to be any unandled queries waiting.
   
   declare
      use CLI;
      use UI_Primitives;
   begin
      New_Line;
      Put_Warn_Tag;
      Put_Line (" DEPRECIATED FEATURE WARNING", Style => Bold + Yellow_FG);
      
      Put_Empty_Tag;
      Put_Line
        (" This project has aura subsystem sources in the project root.");
      Put_Empty_Tag;
      Put_Line
        (" That is now a depreciated project structure. For this and later");
      Put_Empty_Tag;
      Put_Line
        (" verions of AURA CLI, all aura subsystem sources must be in the");
      Put_Empty_Tag;
      Put_Line
        (" 'aura' subdirectory for AURA CLI to proceed.");
      
      New_Line;
      Put_Empty_Tag;
      Put_Line
        (" AURA CLI can move these files for you, but if you elect not to,");
      Put_Empty_Tag;
      Put_Line
        (" this version of the AURA CLI will not be able to proceed.");
      New_Line;
      
   end;
   
   loop
      select
         -- We shouldn't need to do this
         User_Queries.Query_Manager.Start_Query;
         Query_Active := True;
      else
         raise Program_Error with "Unexpected: user query active";
      end select;
      
      User_Queries.Query_Manager.Post_Query
        (Prompt        => "Move all aura subsystems sources to 'aura'? (y/n)",
         Default       => "y",
         Response_Size => 1);
      
      User_Queries.Query_Manager.Take_Query
        (UI_Primitives.Query_Driver'Access);
      
      declare
         Query_Response: String (1 .. 1);
         Last: Natural;
      begin
         -- Since we are doing this all from a single thread, this should
         -- never block
         select
            User_Queries.Query_Manager.Wait_Response
              (Response => Query_Response,
               Last     => Last);
            User_Queries.Query_Manager.End_Query;
            Query_Active := False;
         else
            raise Program_Error with "Unexpected: query response lost.";
         end select;
         
         if Last = 1 then
            case Query_Response(1) is
               when 'y' | 'Y' =>
                  exit;
                  
               when 'n' | 'N' =>
                  UI_Primitives.Put_Info_Tag;
                  CLI.Put_Line (" All aura units must be moved to the 'aura' "
                                  & "subdirectory to continue.");
                  UI_Primitives.Put_Empty_Tag;
                  CLI.Put_Line (" AURA CLI will now abort.");
                  CLI.New_Line;
                  OK_To_Proceed := False;
                  return;
                  
               when others =>
                  UI_Primitives.Put_Info_Tag;
                  CLI.Put_Line (" You must answer y or n");
                  CLI.New_Line;
            end case;
         end if;
      end;
   end loop;
   
   -- We have been given the go-ahead by the user.
   
   Process_Changes;
   -- Process_Changes sets OK_To_Proceed as appropriate
   
   
exception
   when e: others =>
      if Query_Active then
         User_Queries.Query_Manager.End_Query;
      end if;
      
      CLI.New_Line;
      UI_Primitives.Put_Fail_Tag;
      CLI.Put_Line ("Unexpected exception: " 
                      & Ada.Exceptions.Exception_Information (e));
      UI_Primitives.Put_Empty_Tag;
      CLI.Put_Line ("Aborting.");
      OK_To_Proceed := False;
   
end AURA_Subdirectory;
