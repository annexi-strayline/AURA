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

with Ada.Text_IO;
with Ada.Streams;
with Ada.Directories;
with Ada.Strings.Bounded;
with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Platform_Info;
with Child_Processes;
with Child_Processes.Path_Searching;
with Child_Processes.Wait_And_Buffer;

separate (Configuration)

procedure Step_3b (Target: in out Subsystem) is
   
   package ACC renames Ada.Characters.Conversions;
   package UTF_8 renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   package Subsystems renames Registrar.Subsystems;
   
   Config_Package_Name: constant Wide_Wide_String
     := Config_Unit_Name (Target).To_String;
   
   Config_Build_Root: constant String := "./.aura/cfg_tmp"
     & '/' & ACC.To_String (Target.Name.To_String);
   
   package Program_Paths renames Child_Processes.Path_Searching;
   
   GNAT_Make_Program: aliased constant String := "gnatmake";
   GNAT_Make: constant Program_Paths.Elaboration_Path_Search
     := Program_Paths.Initialize (GNAT_Make_Program);
   
   -- Note that this is for the native gnatmake, not the target gnatmake.
   -- We are using it to compile the generated extraction program that
   -- retrieves the configuration values from the AURA package configuration,
   -- hence we don't append the Toolchain_Prefix.
   --
   -- Obviously this is GNAT-specific, one day it might make sense to
   -- hand-write an internal compile-bind-link subprogram to make AURA more
   -- portable
   
   -- Wait_And_Buffer instantiation
   
   package Output_Buffers is 
     new Ada.Strings.Bounded.Generic_Bounded_Length (2048);
   
   procedure Buffer_Append (Buffer: in out Output_Buffers.Bounded_String;
                            Item  : in     String)
   is begin
      Output_Buffers.Append (Source => Buffer, New_Item => Item);
   end Buffer_Append;
   
   procedure Wait_And_Buffer is new Child_Processes.Wait_And_Buffer
     (Buffer_Type  => Output_Buffers.Bounded_String,
      Append       => Buffer_Append,
      Empty_Buffer => Output_Buffers.Null_Bounded_String);
   
   
   -- These are to catch errors in the compilation process, mostly
   
   package TIO renames Ada.Text_IO;
   
   Out_File: TIO.File_Type;   
   Tab: constant String := (1 .. 4 => ' ');
   
   procedure NL (File   : in TIO.File_Type      := Out_File;
                 Spacing: in TIO.Positive_Count := 1)
     renames TIO.New_Line;
   
   procedure Put (Item: in String;
                  File: in TIO.File_Type := Out_File)
   is begin
      TIO.Put (File, Item);
   end Put;
   
   
   procedure Gen_Loader (Package_Name: Wide_Wide_String; 
                         Configs     : Subsystems.Configuration_Vector) 
   is
      -- String'Output (STDOUT, AURA.Subsystem.Package_Name.Element);
      Target_Prefix: constant String 
        := UTF_8.Encode (Config_Package_Name & '.' & Package_Name);
   begin
      for Item of Configs loop
         Put (Tab & "String'Output (STDOUT, " & Target_Prefix & '.' 
                & UTF_8.Encode (WWU.To_Wide_Wide_String (Item.Name))
                & ");");
         NL;
      end loop;
      
      NL;
   end Gen_Loader;
   
   
   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Configs: in out Subsystems.Configuration_Vector)
   is 
      use Subsystems.Configuration_Vectors;
   begin
      for Item of Configs loop
         Item.Value := UBS.To_Unbounded_String (String'Input (Stream));
      end loop;
      
      -- Now remove any items with an empty value, start from the "bottom up"
      -- to try to make it a little more efficient
      
      for I in reverse Configs.First_Index .. Configs.Last_Index loop
         if UBS.Length (Configs(I).Value) = 0 then
            Configs.Delete (I);
         end if;
      end loop;
   end Load;
   
begin
   
   declare
      use Ada.Directories;
   begin
      if Exists (Config_Build_Root) then
         Delete_Tree (Config_Build_Root);
      end if;
      Create_Path (Config_Build_Root);
   end;
   
   -- Phase one: generate the extraction program
   
   TIO.Create (File => Out_File,
               Mode => TIO.Out_File,
               Name => Config_Build_Root & '/' & "extract.adb");
   Put ("with " & UTF_8.Encode (Config_Package_Name) & ';');   NL;
   Put ("with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;"); NL;
   
   NL;
   
   Put ("procedure Extract is"); NL;
   Put (Tab & "STDOUT: constant Stream_Access ");
   Put (":= Stream (Ada.Text_IO.Standard_Output);"); NL;
   Put ("begin"); NL;
   
   NL;
   
   Gen_Loader ("build.external_libraries",
               Target.Configuration.External_Libraries);
   
   Gen_Loader ("build.ada.compiler_options",
               Target.Configuration.Ada_Compiler_Opts);
   
   Gen_Loader ("build.c.compiler_options", 
               Target.Configuration.C_Compiler_Opts);
   
   Gen_Loader ("build.c.preprocessor_definitions",
               Target.Configuration.C_Definitions);
   
   Gen_Loader ("codepaths",
               Target.Configuration.Codepaths);
   
   Gen_Loader ("information",
               Target.Configuration.Information);
   
   Put ("end Extract;"); NL;
   TIO.Close (Out_File);
   
   -- Phase two: compile the extraction program
   
   declare
      use Child_Processes;
      use Output_Buffers;
      
      STDOUT, STDERR: Bounded_String;
      Compiler: Child_Process'Class := Spawn_Process
        (Image_Path        => Program_Paths.Image_Path (GNAT_Make),
         Arguments         => "-I../../../aura/ extract.adb",
         Working_Directory => Config_Build_Root);
      
      Timed_Out: Boolean;
      Status   : Exit_Status;
   begin
      Wait_And_Buffer (Process   => Compiler,
                       Poll_Rate => 0.1,
                       Timeout   => 120.0,
                       Output    => STDOUT,
                       Error     => STDERR,
                       Timed_Out => Timed_Out,
                       Status    => Status);
      
      Assert (Check   => not Timed_Out and then Status = Success,
              Message => New_Line 
                &        "Compilation of the configuration extraction " 
                &        "program failed" & (if Timed_Out then 
                                                "(Timed out)" 
                                             else 
                                                "")
                &        ':' & New_Line 
                &        To_String (STDERR));
   end;
   
   -- Phase three: run the extraction program
   
   declare
      use Child_Processes;
      use Output_Buffers;
      
      STDERR_Buffer: Bounded_String;
      
      Extractor: Child_Process'Class := Spawn_Process
        (Image_Path        => Config_Build_Root & "/extract",
         Arguments         => "",
         Working_Directory => Ada.Directories.Current_Directory);
      
      Timed_Out: Boolean;
      Status   : Exit_Status;
   begin
      Extractor.Set_Stream_Timeout (Selector => Standard_Output,
                                    Timeout  => 30.0);
      
      declare
         STDOUT: constant not null access Ada.Streams.Root_Stream_Type'Class
           := Extractor.IO_Stream (Standard_Output);
      begin
         Load (STDOUT, Target.Configuration.External_Libraries);
         Load (STDOUT, Target.Configuration.Ada_Compiler_Opts );
         Load (STDOUT, Target.Configuration.C_Compiler_Opts   );
         Load (STDOUT, Target.Configuration.C_Definitions     );
         Load (STDOUT, Target.Configuration.Codepaths         );
         Load (STDOUT, Target.Configuration.Information       );
      end;
      
      Extractor.Wait_Terminated (Timeout   => 10.0,
                                 Timed_Out => Timed_Out,
                                 Status    => Status);
      
      if Timed_Out  then
         raise Ada.Assertions.Assertion_Error with
           "Extractor program timed-out";
         
      elsif Status /= Success then
         -- Attempt to flush STDERR
         
         declare
            STDERR: constant not null access Ada.Streams.Root_Stream_Type'Class
              := Extractor.IO_Stream (Standard_Error);
            C: Character;
         begin
            loop
               Character'Read (STDERR, C);
               Append (Source   => STDERR_Buffer,
                       New_Item => C);
            end loop;
         exception
            when others => null;
         end;
         
         raise Ada.Assertions.Assertion_Error with
            "Extractor program failed:" & New_Line 
           & To_String (STDERR_Buffer);
      end if;
      
   end;
   
   Step_4 (Target);
end Step_3b;
