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

with Ada.Text_IO;

with Platform_Info;
with Registrar.Registration;

separate (Repositories)

procedure Generate_AURA_Spec is
   use type Ada.Directories.File_Kind;
   package TIO renames Ada.Text_IO;
   package FIO is new TIO.Enumeration_IO (Repository_Format);
   
   Spec: TIO.File_Type;
   
   procedure Put (Item: in String) with Inline is
   begin
      TIO.Put (File => Spec, Item => Item);
   end;
   
   
   procedure Put (Item: in Repository_Format) with Inline is
   begin
      FIO.Put (File => Spec,
               Item => Item,
               Set  => TIO.Lower_Case);
   end;
   
   procedure Put_Line (Item: in String) with Inline is
   begin
      TIO.Put_Line (File => Spec, Item => Item);
   end;
                      
   procedure New_Line (File   : in TIO.File_Type := Spec;
                       Spacing: in TIO.Positive_Count := 1)
     renames TIO.New_Line;
   
   -- We cannot use TIO.Set_Output since this is a very multi-tasking program,
   -- and the CLI driver will inject random stuff into the spec if we try.
   -- I know because I tried.
                 
   Tab: constant String := (1 .. 4 => ' ');
   
   Spec_Directory: constant String := Ada.Directories.Compose
     (Containing_Directory => Ada.Directories.Current_Directory,
      Name                 => "aura");
   
   Spec_File_Name: constant String := Ada.Directories.Compose
     (Containing_Directory => Spec_Directory,
      Name                 => "aura.ads");
   
begin
   
   if not Ada.Directories.Exists (Spec_Directory) then
      Ada.Directories.Create_directory (Spec_Directory);
      
   else
      Assert (Check => Ada.Directories.Kind (Spec_Directory)
                = Ada.Directories.Directory,
              Message => "The 'aura' name of an aura project should be a "
                &        "directory.");
   end if;
   
   -- Given that the directory exists, we expect that there is no "aura.ads"
   -- there since this should have been entered into the repo, and if it has
   -- been, this procedure should never get called.
   
   TIO.Create (File => Spec,
               Name => Spec_File_Name);
   
   Put_Line 
     ("-- This specification was automatically generated by the AURA CLI");
   
   Put_Line
     ("-- DO NOT MODIFY");
   
   New_Line;
   Put_Line ("package AURA with Pure is");
   
   -- Repository format
   New_Line;
   Put (Tab & "type Repository_Format is (");
   
   for Format in Repository_Format loop
      if Format /= Repository_Format'First then
         Put (", ");
      end if;
      
      Put (Format);
   end loop;
   
   Put_Line (");");
   New_Line;
   
   -- Platform info
   declare
      package PI renames Platform_Info;
      
      procedure CS (Name, Value: in String) is
      begin
         Put (Tab & Name & ": constant String := """ & Value & """;");
         New_Line;
      end CS;
   begin
      CS (Name => "Platform_Family",       Value => PI.Platform_Family);
      CS (Name => "Platform_Flavor",       Value => PI.Platform_Flavor);
      CS (Name => "Platform_Version",      Value => PI.Platform_Version);
      CS (Name => "Platform_Architecture", Value => PI.Platform_Architecture);
   end;
   
   New_Line;
   Put ("end AURA;");
   
   TIO.Close (Spec);

   -- Now that the spec file has been created, we need to find the
   -- directory entry to submit to Enter_Unit
   
   declare
      use Ada.Directories;
      
      Search: Search_Type;
      Spec  : Directory_Entry_Type;
   begin
      Start_Search (Search    => Search,
                    Directory => Spec_Directory,
                    Pattern   => "aura.ads");
      
      Assert (Check   => More_Entries (Search),
              Message => "Error generating AURA spec - cannot find "
                &        "generated file!");
      
      Get_Next_Entry (Search => Search, Directory_Entry => Spec);
      Registrar.Registration.Enter_Unit (Spec);
      End_Search (Search);
   end;
   
   
end Generate_AURA_Spec;
