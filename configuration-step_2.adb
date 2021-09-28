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
with Ada.Streams.Stream_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Registrar.Source_Files;
with Unicode.UTF8_Stream_Decoder;
with Unicode.Case_Folding.Simple;

separate (Configuration)

-- Step 2 - we are creating a new configuration unit from the manifest.
--
-- This means both that the registry does not contain a unit corresponding to
-- the configuration unit, and that the registry does cointain a unit
-- corresponding to the manifest. We need to process the entire manifest unit.
--
-- The biggest part of this job is replacing the manifest's package name from
-- Target.AURA to AURA.Target. We will do text substituation, but we need to
-- comply with the Ada standard to 1) allow unicode (utf-8) identifiers, and
-- 2) apply case-folding.
--
-- The Ada standard requires all content to be in Normalization Form 'C' (202X)
-- but we don't need to check for that here as the Ada Lexical Parser will
-- pick that out during unit entry

procedure Step_2 (Target: in out Subsystem) is
   
   use type Registrar.Source_Files.Source_File_Access;
   
   -- These are already case folded
   
   Manifest_Name: constant Unit_Name := Manifest_Unit_Name (Target);
   Config_Name  : constant Unit_Name := Config_Unit_Name   (Target);
   
   -- The manifest has to exist before a call to Step_2, so we don't need
   -- to worry about pulling it out of the registry.
   
   Manifest: constant Library_Unit
     := Reg_Qs.Lookup_Unit (Manifest_Name);
   
   -- The basic gyst is: we are reading the manifest as a utf-8 stream,
   -- looking for Expected_Name, and if we find it (we should at least
   -- one), we replace that in the output stream with Substitute name.
   -- Everything else passes through unchanged.
   
   Expected_Name: constant Wide_Wide_String
     := Manifest_Name.To_String;
   
   Substitute_Name: constant Wide_Wide_String
     := Config_Name.To_String;
   
   Scan_Buffer : Wide_Wide_String (1 .. Expected_Name'Length);
   Match_Buffer: Wide_Wide_String (Scan_Buffer'Range);
   Match_Depth: Positive;
   
   procedure Find_And_Replace 
     (In_Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Out_Stream: not null access Ada.Streams.Root_Stream_Type'Class)
   is
      subtype UTF_8_String is Ada.Strings.UTF_Encoding.UTF_8_String;
      use type UTF_8_String;
      
      package UTF_8 renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      
      function Decode_Next 
        (UTF8_Stream: not null access Ada.Streams.Root_Stream_Type'Class 
           := In_Stream)
        return Wide_Wide_Character 
        renames Unicode.UTF8_Stream_Decoder.Decode_Next;

      
   begin
      Match_Depth := Scan_Buffer'First;
      
      loop
         -- The Scan_Buffer will hold the source while we see if it matches
         -- the expected name, however to match accoring to the Ada rules,
         -- we need to first do simple case folding before checking for a
         -- match. Since we don't want to apply that case folding to the
         -- source itself, we need to have a parallel case-folded buffer
         
         Scan_Buffer(Match_Depth) := Decode_Next;
         Match_Buffer(Match_Depth) 
           := Unicode.Case_Folding.Simple (Scan_Buffer(Match_Depth));
         
         if Match_Buffer(Match_Depth) = Expected_Name(Match_Depth) then
            
            if Match_Depth = Scan_Buffer'Last then
               -- It is not really worth the effort to check for weird things
               -- showing up where this naieve replace might mangle the middle
               -- of something that happens to match. It should be rare enough
               -- (especially in a manifest) to not often cause a problem.
               --
               -- The Ada compiler will almost certainly spot it later.
               --
               -- If we wanted to check for that, we'd really want to bring
               -- in the Ada lexical parser since we really should not be
               -- handling the various rules ourselves and creating redundant
               -- code. However using the parser would mess up our stream.
               --
               -- We considered possibly "rewriting" the unit via the parser,
               -- but since the Configuration unit really should be human
               -- -readable and human-changable, this seemed like the wrong
               -- approach.
               --
               -- If this really becomes a problem, we can always make
               -- improvements.
               
               UTF_8_String'Write
                 (Out_Stream,
                  UTF_8.Encode (Substitute_Name));
               Match_Depth := Scan_Buffer'First;
               
            else
               -- Keep trying
               Match_Depth := Match_Depth + 1;
            end if;
            
         else
            -- Flush buffer
            UTF_8_String'Write 
              (Out_Stream, 
               UTF_8.Encode (Scan_Buffer(Scan_Buffer'First .. Match_Depth)));
            
            Match_Depth := Scan_Buffer'First;
         end if;
         
      end loop;
      
   exception
      when Ada.Streams.Stream_IO.End_Error =>
         -- The Ada lexical parser on entry is not used to dig down through the
         -- whole spec, lets do a little check for funny business while we're
         -- at it (we shouldn't get here if we were in the middle of the
         -- Expected_Name)
         
         Assert (Check  => Match_Depth = Scan_Buffer'First,
                 Message => "Unexpected end of manifest");
         
         return;
         
      when others =>
         raise;
      
   end Find_And_Replace;
   
   
   procedure Register (Name: in String) is
      use Ada.Directories;
      
      Search : Search_Type;
      New_Reg: Directory_Entry_Type;
   begin
      Start_Search (Search    => Search,
                    Directory => Current_Directory,
                    Pattern   => Name);
      
      Assert (Check   => More_Entries (Search),
              Message => "Error registering configuration unit - cannot find "
                &        "generated file " & Name & '!');
      
      Get_Next_Entry (Search => Search, Directory_Entry => New_Reg);
      Registrar.Registration.Enter_Unit (New_Reg);
      End_Search (Search);
      
   end Register;
   
begin
   -- The following could be generic, but the parameters would be pretty
   -- complex, and the code is so simple, it doesn't seem worth it.

   -- Spec
   declare
      use Ada.Streams.Stream_IO;
      use Registrar.Source_Files;
      
      Spec_Name: constant String 
        := "aura-" & Target.Name.To_UTF8_String & ".ads";
      File     : File_Type;
      M_Stream : aliased Source_Stream 
        := Checkout_Read_Stream (Manifest.Spec_File);
   begin
      Create (File => File,
              Mode => Out_File,
              Name => Spec_Name);
   
      Find_And_Replace (In_Stream  => M_Stream'Access,
                        Out_Stream => Stream (File));
      
      Close (File);
      Register (Spec_Name);
   end;
   
   if Manifest.Body_File /= null then
      -- Body
      declare
         use Ada.Streams.Stream_IO;
         use Registrar.Source_Files;
         
         Body_Name: constant String 
           := "aura-" & Target.Name.To_UTF8_String & ".adb";
         File     : File_Type;
         M_Stream : aliased Source_Stream 
           := Checkout_Read_Stream (Manifest.Body_File);
      begin
         Create (File => File,
                 Mode => Out_File,
                 Name => Body_Name);
         
         Find_And_Replace (In_Stream  => M_Stream'Access,
                           Out_Stream => Stream (File));
         
         Close (File);
         Register (Body_Name);
      end;
   end if;
   
   -- Next is Step 3, but we need to wait for the registrar to register the
   -- configuration unit before we can process it, so we need to defer
   -- Step_3

   Workers.Disable_Completion_Reports;
   Configuration_Progress.Increment_Total_Items;
   Workers.Defer_Order
     (Order => Step_3a_Deferral'(Tracker => Configuration_Progress'Access,
                                 Target  => Target),
      Wait_Tracker => Registrar.Registration.Entry_Progress'Access);

   
end Step_2;
