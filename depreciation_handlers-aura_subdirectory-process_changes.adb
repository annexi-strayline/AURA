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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Registrar.Source_Files;
with Registrar.Registration.Unchecked_Deregister_Unit;

separate (Depreciation_Handlers.AURA_Subdirectory)

procedure Process_Changes is
   -- This procedure is invoked when we
   -- 1. Do have aura subsystem library units in the project root; and,
   -- 2. Have the go-ahead from the user to act
   
   use Ada.Directories;
   use Registrar.Library_Units;
   use CLI;
   use UI_Primitives;
   
   use type Registrar.Source_Files.Source_File_Access;
   
   package LUKO renames
     Registrar.Library_Units.Library_Unit_Sets_Keyed_Operations;
   
   
   type Move_Registration is
      record
         From, To: Ada.Strings.Unbounded.Unbounded_String;
      end record;
   
   package Move_Registration_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Move_Registration);
   
   
   Move_Registrations: Move_Registration_Vectors.Vector;
   
   AURA_Subsys: constant Registrar.Subsystems.Subsystem
     := Registrar.Queries.Lookup_Subsystem (AURA_Subsystem_Name);
   
   AURA_Units: Library_Unit_Sets.Set
     := Registrar.Queries.Subsystem_Library_Units (AURA_Subsys);
   
   AURA_Target_Directory: constant String
     := Compose (Containing_Directory => Current_Directory,
                 Name                 => "aura");
   
   procedure Bad_Unit_Notice (Unit   : Library_Unit;
                              Message: String) 
   is
   begin
      Put_Fail_Tag;
      Put_Line (" Bad AURA Unit: " & Unit.Name.TO_UTF8_String);         
      Put_Empty_Tag;
      Put_Line (' ' & Message);
   end;
   
   
   procedure Register_Move 
     (Source: not null Registrar.Source_Files.Source_File_Access)
   is
      use Ada.Strings.Unbounded;
      use Registrar.Source_Files;
      
      Source_Full_Name: constant String := Source.Full_Name;
      Target_Full_Name: constant String := Compose
        (Containing_Directory => AURA_Target_Directory,
         Name                 => Simple_Name (Source_Full_Name));

   begin
      Move_Registrations.Append 
        ((From => To_Unbounded_String (Source_Full_Name),
          To   => To_Unbounded_String (Target_Full_Name)));
      

   end Register_Move;
   
   
   procedure Execute_Move (Registration: Move_Registration) is
      use Ada.Strings.Unbounded;
      Source_Full_Name: constant String := To_String (Registration.From);
   begin
      Rename (Old_Name => Source_Full_Name,
              New_Name => To_String (Registration.To));
   exception
      when e: others =>
         OK_To_Proceed := False;
         Put_Fail_Tag;
         Put_Line (" Unable to move " & Simple_Name (Source_Full_Name));
         Put_Empty_Tag;
         Put_Line (' ' & Ada.Exceptions.Exception_Information (e));
   end Execute_Move;
   
begin
   OK_To_Proceed := False;
   
   if Exists (AURA_Target_Directory) then
      if Kind (AURA_Target_Directory) /= Directory then
         Put_Fail_Tag;
         Put_Line (" File named 'aura' exists in the project root and is not "
                     & "a directory.");
         Put_Empty_Tag;
         Put_Line (" Remove that file and try again.");
         return;
         
      else
         Put_Warn_Tag;
         Put_Line (" 'aura' directory already exists in project root.");
         Put_Empty_Tag;
         Put_Line (" you will be prompted to overwrite files if needed.");
         
      end if;
      
   else
      Create_Directory (AURA_Target_Directory);
      Put_Info_Tag;
      Put_Line (" created 'aura' directory in project root to contain all "
                  & "aura subsystem files.");
   end if;
   
   -- Note that all units registered to the aura subsystem have gone through
   -- all the checks of normal unit regisration, which includes special checks
   -- just for members of the AURA subsystem, and implies that either Spec_File
   -- and/or Body_File are non-null
   
   -- Before we can move a file, we need to delink the source file associated
   -- with it, or we can cause an exception when a checkout is attempted during
   -- Unchecked_Deregister_Unit process.
   
   Unit_Loop: for Unit of AURA_Units loop
      if Unit.Spec_File /= null then
         Register_Move (Unit.Spec_File);
      end if;
      
      if Unit.Body_File /= null then
         Register_Move (Unit.Body_File);
      end if;
      
      for Subunit of Unit.Subunit_Bodies loop
         -- If any of these are null, that would be a program error
         Register_Move (Subunit);
      end loop;
   end loop Unit_Loop;
   
   -- With all file moves registered, we can Deregister all registered AURA
   -- units before proceeding with the actual file move
      
   for C in AURA_Units.Iterate loop
      if AURA_Units(C).State = Available then
         LUKO.Update_Element_Preserving_Key
           (Container => AURA_Units,
            Position  => C,
            Process   => Registrar.Registration
              .Unchecked_Deregister_Unit'Access);
      end if;
   end loop;
   
   -- Finally attempt to move as many (hopefully all) of the files as possible.
   
   OK_To_Proceed := True;
   for Registration of Move_Registrations loop
      Execute_Move (Registration);
   end loop;
   
   if OK_To_Proceed then
      Put_OK_Tag;
      Put_Line (Ada.Containers.Count_Type'Image 
                  (Move_Registrations.Length)
                  & " AURA subsystem files relocated to the 'aura' "
                  & "subdirectory.");
   else
      Put_Fail_Tag;
      Put_Line (" AURA subsystem relocation failed or incomplete.");
      Put_Empty_Tag;
      Put_Line (" Please try again after correcting the above errors, or else");
      Put_Empty_Tag;
      Put_Line (" manually relocate all AURA subsystem units to the 'aura'"); 
      Put_Empty_Tag;
      Put_Line (" subdirectory, and then re-run AURA.");
   end if;
   
end Process_Changes;
