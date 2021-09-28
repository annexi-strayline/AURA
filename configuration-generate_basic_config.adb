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

-- This specification was automatically generated by the AURA CLI since the
-- AURA subsystem has no manifest. 

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Characters.Latin_1;

separate (Configuration)

procedure Generate_Basic_Config (Target: in out Subsystem) is
   
   procedure Generate
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
   is
      procedure NL is
      begin
         Character'Write (Stream, New_Line);
      end NL;
      
      procedure Put_Line  (Item: in String) is
      begin
         String'Write (Stream, Item); NL;
      end Put_Line;
      
   begin
      Put_Line
        ("-- This specification was automatically generated by the AURA CLI " 
           & "since the");
      
      Put_Line
        ("-- AURA subsystem has no manifest. ");
      
      NL;
      Put_Line ("package " 
                  & Config_Unit_Name (Target).To_UTF8_String 
                  & " is end;");

   end Generate;
   
   use Ada.Streams.Stream_IO;
   
   Spec_File: File_Type;
   File_Name: constant String := "aura-" & Target.Name.To_UTF8_String & ".ads";
   
begin
   -- It would be a surprise for this file to exist.
   Create (File => Spec_File,
           Name => File_Name);
   Generate (Stream (Spec_File));
   Close (Spec_File);
   
   -- Enter with the registrar
   declare
      use Ada.Directories;
      
      Search   : Search_Type;
      Conf_Unit: Directory_Entry_Type;
   begin
      Start_Search (Search    => Search,
                    Directory => Current_Directory,
                    Pattern   => File_Name);
      
      Assert (Check   => More_Entries (Search),
              Message => "Error generating configuration unit - cannot find "
                &        "generated file!");
      
      Get_Next_Entry (Search => Search, Directory_Entry => Conf_Unit);
      Registrar.Registration.Enter_Unit (Conf_Unit);
      End_Search (Search);
   end;
   
end Generate_Basic_Config;