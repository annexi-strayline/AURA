------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
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

with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Ada.Characters.Conversions;

with Registrar.Registration;

separate (Repositories.Repo_Spec_Handling)

procedure Generate_Repo_Spec (Index: Repository_Index) is

   use Unit_Names;
   
   function To_Wide_Wide_String (Item: in String) return Wide_Wide_String
     renames Ada.Characters.Conversions.To_Wide_Wide_String;
   
   Repo: constant Repository := All_Repositories.Extract (Index);
   
   Tab: constant String := (1 .. 4 => ' ');
   
   Trimmed_Index: constant String 
     := Ada.Strings.Fixed.Trim (Source => Repository_Index'Image(Index),
                                Side   => Ada.Strings.Both);
   
   Spec_Directory: constant String := Ada.Directories.Compose
     (Containing_Directory => Ada.Directories.Current_Directory,
      Name                 => "aura");
   File_Base_Name: constant String
     := "aura-repository_" & Trimmed_Index & ".ads";
   File_Full_Name: constant String := Ada.Directories.Compose
     (Containing_Directory => Spec_Directory,
      Name                 => File_Base_Name);
   
   Format_Value: constant String
     := Ada.Characters.Handling.To_Lower 
       (Repository_Format'Image (Repo.Format));
   
   
   procedure Generate
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
   is
      procedure New_Line is
      begin
         Character'Write (Stream, Ada.Characters.Latin_1.LF);
      end New_Line;
      
      procedure Put_Line  (Item: in String) is
      begin
         String'Write (Stream, Item);
         New_Line;
      end Put_Line;
      
   begin
      
      Put_Line 
        ("-- This specification was automatically generated by the AURA CLI");
      
      Put_Line
        ("-- DO NOT MODIFY THIS FILE MANUALLY");
      
      New_Line;
      Put_Line ("package AURA.Repository_" & Trimmed_Index & " with Pure is");
      
      New_Line;
      Put_Line (Tab & "Format  : constant Repository_Format := " 
                  & Format_Value & ';');
      
      Put_Line (Tab & "Location: constant String := """
                  & UBS.To_String (Repo.Location) & """;");
      
      if UBS.Length (Repo.Snapshot) > 0 then
         Put_Line (Tab & "Snapshot: constant String := """
                     & UBS.To_String (Repo.Snapshot)
                     & """;");
      end if;
      
      case Repo.Format is
         when System | Local =>
            -- Nothing left to add
            null;
            
         when Git =>
            if UBS.Length (Repo.Tracking_Branch) > 0 then
               New_Line;
               Put_Line (Tab & "Tracking_Branch: constant String := """
                           & UBS.To_String (Repo.Tracking_Branch)
                           & """;");
            end if;
      end case;
      
      New_Line;
      Put_Line ("end AURA.Repository_" & Trimmed_Index & ';');
      
   end Generate;
   
   
   Lookup_Name: Unit_Name;
   
begin
   -- See if the repo spec has already been entered by the Registrar.
   Lookup_Name.Set_Name 
     ("AURA.Repository_" & To_Wide_Wide_String (Trimmed_Index));
   
   if Reg_Qs.Unit_Entered (Lookup_Name) then
      declare
         use Registrar.Library_Units;
         use Registrar.Source_Files;
         
         Existing_Spec: constant Library_Unit
           := Reg_Qs.Lookup_Unit (Lookup_Name);
         
         Spec_Source: aliased Source_Stream
           := Checkout_Write_Stream (Source  => Existing_Spec.Spec_File,
                                     Rewrite => True);
      begin
         Generate (Spec_Source'Access);
      end;
      
   else
      -- New repo from scratch
      
      declare
         use Ada.Streams.Stream_IO;
         
         New_Spec: File_Type;
      begin
         Create (File => New_Spec,
                 Name => File_Full_Name);
         Generate (Stream (New_Spec));
         Close (New_Spec);
      end;

      
      -- Now that the spec file has been created, we need to find the
      -- directory entry to submit to Enter_Unit
      
      declare
         use Ada.Directories;
         
         Search : Search_Type;
         Specent: Directory_Entry_Type;
      begin
         Start_Search (Search    => Search,
                       Directory => Spec_Directory,
                       Pattern   => File_Base_Name);
         
         Assert (Check   => More_Entries (Search),
                 Message => "Error generating Repository spec - cannot find "
                   &        "generated file!");
         
         Get_Next_Entry (Search => Search, Directory_Entry => Specent);
         Registrar.Registration.Enter_Unit (Specent);
         End_Search (Search);
      end;
   end if;
   
end Generate_Repo_Spec;
