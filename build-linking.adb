------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2023, ANNEXI-STRAYLINE Inc.                          --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Assertions;
with Ada.Directories;
with Ada.Environment_Variables;

with Platform_Info;
with Registrar.Queries;
with Registrar.Registration;
with Registrar.Subsystems;
with Registrar.Library_Units;
with Workers, Workers.Reporting;
with Child_Processes.Path_Searching;

package body Build.Linking is
   
   New_Line: Character renames Workers.Reporting.New_Line;
   
   procedure Assert (Check: Boolean; Message: in String)
     renames Ada.Assertions.Assert;
   
   package Program_Paths renames Child_Processes.Path_Searching;
   
   Binder_Program: aliased constant String 
     := (Platform_Info.Toolchain_Prefix & "gnatbind");
   Binder: constant Program_Paths.Elaboration_Path_Search
     := Program_Paths.Initialize (Binder_Program);
   
   Linker_Program: aliased constant String
     := Platform_Info.Toolchain_Prefix & "gcc";
   Linker: constant Program_Paths.Elaboration_Path_Search
     := Program_Paths.Initialize (Linker_Program);
   
   Archiver_Program: aliased constant String
     := Platform_Info.Toolchain_Prefix & "ar";
   Archiver: constant Program_Paths.Elaboration_Path_Search
     := Program_Paths.Initialize (Archiver_Program);
   
   --
   -- Scan_ALI_Order
   --
   
   type Scan_ALI_Order is new Workers.Work_Order with
      record
         Target: Registrar.Library_Units.Library_Unit;
      end record;
   
   overriding function  Image (Order: Scan_ALI_Order) return String;
   overriding procedure Execute (Order: in out Scan_ALI_Order);
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Scan_ALI_Order) return String is
     ("[Scan_ALI_Order] (Build.Scan_Linker_Options)" & New_Line
        & "Target: " & Order.Target.Name.To_UTF8_String);
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Scan_ALI_Order) is
      use Ada.Text_IO;
      use Registrar.Library_Units;
      
      -- Avoid using the secondary stack, for efficincy
      Buffer: String (1 .. 1920);
      Last  : Natural;
      
      ALI_File: File_Type;
   begin
      pragma Assert (Order.Target.State = Compiled);
      
      -- Skip units that are not Ada Library Units
      if Order.Target.Kind not in Package_Unit | Subprogram_Unit then
         return;
      end if;
      
      Open (File => ALI_File,
            Mode => In_File,
            Name => ALI_File_Name (Order.Target));
      
      -- Fairly simple operation. We'll keep going until we hit the end of the
      -- file, scanning each line. Linker option lines start with 'L', and then
      -- a space, and then a quote-enclosed string. We simply take those strings,
      -- strip the quotes, and slap them onto the queue
      
      while not End_Of_File (ALI_File) loop
         Get_Line (File => ALI_File,
                   Item => Buffer,
                   Last => Last);
         
         if Last > Buffer'First
           -- if Last = Item'First, we definately don't want it anyways
           and then Buffer(1) = 'L' 
         then

            pragma Assert (Buffer(3) = '"');
            pragma Assert (Buffer(Last) = '"');
            
            Linker_Options.Enqueue 
              (UBS.To_Unbounded_String (Buffer(4 .. Last - 1)));
            -- L "option"
            --    ^....^
            -- 1234.....Last
         end if;
      end loop;
      
      Close (ALI_File);
      
   exception
      when others =>
         if Is_Open (ALI_File) then Close (ALI_File); end if;
         raise;
      
   end Execute;
   
   -------------------------
   -- Scan_Linker_Options --
   -------------------------
   
   procedure Scan_Linker_Options 
     (Unit_Set: in Registrar.Library_Units.Library_Unit_Sets.Set)
   is
      use Registrar.Library_Units;
      
      New_Order: Scan_ALI_Order := (Tracker => Scan_Progress'Access,
                                    others  => <>);
   begin
      Scan_Progress.Increase_Total_Items_By (Natural (Unit_Set.Length));
      
      for Unit of Unit_Set loop
         New_Order.Target := Unit;
         Workers.Enqueue_Order (New_Order);
      end loop;
   end Scan_Linker_Options;
   
   ----------
   -- Bind --
   ----------
   
   procedure Bind
     (Unit_Set     : in     Registrar.Library_Units.Library_Unit_Sets.Set;
      Configuration: in     Build_Configuration;
      Errors       :    out UBS.Unbounded_String)
   is
      use UBS;
      use Registrar.Library_Units;
      
      use type Ada.Containers.Count_Type;
      
      Need_GNARL: Boolean := False;
      -- GNAT-specific. GNARL is the tasting part of the Ada runtime. Not
      -- all programs need this, and those that do also need pthreads
      
      Args: Unbounded_String;
      Bind_Output: Unbounded_String;
      
   begin
      pragma Assert (Configuration.Mode in Library | Image);
      pragma Assert 
        (if Unit_Set.Length = 1 then 
            Configuration.Mode = Image 
            and then Unit_Set(Unit_Set.First).Kind = Subprogram_Unit);
      pragma Assert (for all Unit of Unit_Set => 
                       Unit.Kind in Package_Unit | Subprogram_Unit);
      
      
      -- Verify that we have a binder
      if not Program_Paths.Found (Binder) then
         raise Program_Error with
           "Bind failed: Could not find the binder program (" 
           & Binder_Program & ").";
      end if;
      
      -- Generate the binder file
      
      -- Switches first
      Set_Unbounded_String (Args, "-x -o ada_main.adb");
      
      if Configuration.Mode = Library then
         Append (Args, " -n"); 
      elsif Unit_Set.Length > 1 then
         Append (Args, " -z");
      end if;
      
      if Configuration.Linking in Static | Static_RT then
         Append (Args, " -static");
      else
         Append (Args, " -shared");
      end if;
      
      
      -- Now add the unit ALI file names
      for Unit of Unit_Set loop
         Append (Args, ' ' & ALI_File_Name (Unit));
      end loop;
      
      -- Keep a record of our command
      declare
         use Ada.Text_IO;
         
         Path: constant String := Build_Output_Root & "/ada_main.binder.cmd";
         
         CMD_OUT: File_Type;
      begin
         if Ada.Directories.Exists (Path) then
            Open (File => CMD_OUT,
                  Mode => Out_File,
                  Name => Path);
         else
            Create (File => CMD_OUT,
                    Name => Path);
         end if;
         
         Put_Line (CMD_OUT, "Binder used:");
         Put_Line (CMD_OUT, Program_Paths.Image_Path (Binder));
         Put_Line (CMD_OUT, "Arguments used:");
         Put_Line (CMD_OUT, To_String (Args));
         Close (CMD_OUT);
      end;
      
      -- Execute
      declare
         use Child_Processes;
         
         Bind_Process: Child_Process'Class
           := Spawn_Process
             (Image_Path        => Program_Paths.Image_Path (Binder),
              Arguments         => To_String (Args),
              Working_Directory => Build_Root);
         
         Timed_Out: Boolean;
         Status   : Exit_Status;
         Output   : Unbounded_String;
      begin
         Output := Null_Unbounded_String;
         Wait_And_Buffer (Process    => Bind_Process,
                          Poll_Rate  => 0.1,
                          Timeout    => 300.0,
                          Output     => Output,
                          Error      => Errors,
                          Timed_Out  => Timed_Out,
                          Status     => Status);
         if Timed_Out then
            Bind_Process.Kill;
            Append (Errors, " [TIMED OUT]");
         elsif Status = Failure or else Length (Errors) > 0 then 
            if Length (Errors) = 0 then
               Append (Errors, "[No error output]");
            end if;
            
            return;
         end if;
      end;
      
      -- Successful. Errors is empty. 
      -- Complete by entering the outputed binder unit      
      
      declare
         use Ada.Directories;
         
         Search     : Search_Type;
         Unit_Source: Directory_Entry_Type;
      begin
         Start_Search (Search    => Search,
                       Directory => Build_Root,
                       Pattern   => "ada_main.ad*");
         
         -- We are expecting exactly two entries
         for I in 1 .. 2 loop
            if not More_Entries (Search) then
               Set_Unbounded_String 
                 (Errors, "Could not find the expected binder output.");
               return;
            end if;
            
            Get_Next_Entry (Search          => Search,
                            Directory_Entry => Unit_Source);
            
            Registrar.Registration.Enter_Unit (Unit_Source);
         end loop;
         
         if More_Entries (Search) then
            Set_Unbounded_String (Errors, "Unexpected binder artifacts.");
            return;
         end if;
         
         End_Search (Search);
      end;
   end Bind;

   --
   -- Link Operations
   --
   
   ------------------
   -- Find_Ada_RTS --
   ------------------
   
   -- Path to the Ada RTS
   
   function Find_Ada_RTS return String is
      -- This function is very GCC-specific. We want to find the directory
      -- that contains the actual Ada Run-Time libraries (libgnat and libgnarl)
      -- These always reside in the location of libgcc, within the directory
      -- adalib.
      
      use UBS;
      use Ada.Directories;
      use Child_Processes;
      
      GCC_Info: Child_Process'Class 
        := Spawn_Process
          (Image_Path        => Program_Paths.Image_Path (Linker),
           Arguments         => "-print-libgcc-file-name",
           Working_Directory => Current_Directory);
      
      Output, Error: Unbounded_String;
      Timed_Out    : Boolean;
      Status       : Exit_Status;
      
   begin
      Wait_And_Buffer (Process   => GCC_Info,
                       Poll_Rate => 0.01, -- Expected to be quick
                       Timeout   => 1.0,  -- Generous
                       Output    => Output,
                       Error     => Error,
                       Timed_Out => Timed_Out,
                       Status    => Status);
      
      Assert (Check   => not Timed_Out,
              Message => "gcc timed out unexpectedly.");
      
      Assert (Check   => Status = Success and then Length (Error) = 0,
              Message => "gcc failed unexpectedly.");
      
      -- Output now consists of a "full name" to 'libgcc.a'. The containing
      -- directory of that file contains a directory "adalib", which is what
      -- we need to return
      
      return Containing_Directory (To_String (Output)) & "/adalib";
      
   end Find_Ada_RTS;
   
   
   -----------------
   -- Needs_GNARL --
   -----------------
   
   -- GNAT-specific. Scans the binder body file looking for the presence of
   -- "-lgnarl"
   
   function Needs_GNARL return Boolean is
      use Ada.Text_IO;
      
      File: File_Type;
      Test: Character;
      
      In_Section   : Boolean := False;
      
      Section_Test : constant String := "--  BEGIN Object file/option list";
      Section_Test_Depth: Positive := Section_Test'First;
      
      GNARL_Test : constant String := "-lgnarl";
      GNARL_Test_Depth: Positive := GNARL_Test'First;
      
   begin
      Open (File => File,
            Mode => In_File,
            Name => Build_Root & "/ada_main.adb");
      
      while not End_Of_File (File) loop
         Get (File, Test);
         
         if not In_Section then
            if Test = Section_Test(Section_Test_Depth) then
               
               if Section_Test_Depth = Section_Test'Last then
                  In_Section := True; 
               else
                  Section_Test_Depth := Section_Test_Depth + 1;
               end if;
               
            else
               Section_Test_Depth := Section_Test'First;
            end if;
            
         else
            if Test = GNARL_Test(GNARL_Test_Depth) then
               if GNARL_Test_Depth = GNARL_Test'Last then
                  Close (File);
                  return True;
               else
                  GNARL_Test_Depth := GNARL_Test_Depth + 1;
               end if;
               
            else
               GNARL_Test_Depth := GNARL_Test'First;
            end if;
         end if;
         
      end loop;
      
      -- End of file and we didn't find any -lgnarl
      Close (File);
      Assert (In_Section, "Could not find option list in binder source.");
      return False;
      
   exception
      when Name_Error =>
         raise Name_Error with "Binder source was not found";
         
   end Needs_GNARL;
   
   
   ------------------------
   -- Add_Linker_Options --
   ------------------------
   
   -- A generalized procedure for adding all the various linker options that
   -- would be shared between image and library linking
   
   procedure Add_Linker_Options (Configuration: in     Build_Configuration;
                                 Args         : in out UBS.Unbounded_String)
   is
      use UBS;
      
   begin
      
      -- Debug options
      if Configuration.Debug_Enabled then
         Append (Args, " -g");
      end if;
      
      -- User-defined linker options
      declare
         Option: UBS.Unbounded_String;
      begin
         loop
            select
               Linker_Options.Dequeue (Option);
            else
               exit;
            end select;
            
            Append (Args, ' ' & UBS.To_String (Option));
         end loop;
      end;
      
      -- The following options really only apply when we are linking some kind
      -- of final elf "image" - either an executable or a shared library.
      --
      -- For archives (static libraries), we don't want to include these
      -- options in "linker options" output provided along-side the archive
      
      if Configuration.Mode in Image | Systemize
        or else (Configuration.Mode = Library
                   and then Configuration.Linking = Static)
      then

         -- Initial set-up depending on the linking mode
         case Configuration.Linking is
            when Shared =>
               Append (Args, " -shared-libgcc -shared");
               
               -- Only if we are actually creating an executable (image),
               -- should we add the pie/no-pie flags
               
               if Configuration.Mode = Image then
                  if Configuration.Position_Independent then
                     Append (Args, " -pie");
                  else
                     Append (Args, " -no-pie");
                  end if;
               end if;
               
            when Static_RT =>
               Append (Args, " -static-libgcc");
               
            when Static =>
               Append (Args, " -static-libgcc");
               
               if Configuration.Position_Independent then
                  Append (Args, " -static-pie");
               else
                  Append (Args, " -static");
               end if;
               
         end case;
         
         -- If we are building a shared library, we will add in the
         -- initialization and finalization symbols to cause elaboration of the
         -- Ada code via the binder program.
         
         if Configuration.Mode = Library 
           and then Configuration.Linking = Shared
         then
            Append (Args, " -Wl,-init=adainit,-fini=adafinal");
         end if;
         
      end if;
      
   end Add_Linker_Options;
   
   ------------------------
   -- Add_User_Libraries --
   ------------------------
   
   -- Adds "external libraries" defined in all configuration manifests.
   -- These should come after all objects in case any of those libraries are
   -- static. The order of subsystem being arbitary should not be an issue with
   -- static libraries unless one static library depends on another, in which
   -- case it should be included in the configuration manifest in the right
   -- order
   
   procedure Add_User_Libraries (Args: in out UBS.Unbounded_String) is
      use UBS;
   begin
      for Subsys of Registrar.Queries.Available_Subsystems loop
         for Lib_Pair of Subsys.Configuration.External_Libraries loop
            Append (Args, " -l" & To_String (Lib_Pair.Value));
         end loop;
      end loop;
   end Add_User_Libraries;
     
   -----------------
   -- Add_Runtime --
   -----------------
   
   -- Adds the appropriate libraries or archives for the Ada runtime. This
   -- must come after the object list in the case of a static rt
   --
   -- For_Archive is set true when building the linker option outbut for
   -- static archive library builds. This causes only the libgnat and
   -- libgnarl objects to be rolled into the archive
   
   
   procedure Add_Static_Runtime_Archives
     (Configuration: in     Build_Configuration;
      Args         : in out UBS.Unbounded_String;
      GNARL        : in     Boolean := Needs_GNARL;
      RTS_Dir      : in     String  := Find_Ada_RTS)
   is
      -- Add_Static_Runtime_Archives specifically adds the actual archives
      -- for the static version of the Ada runtime. This subprogram is used
      -- both by Add_Runtime and Archive.
      --
      -- Add_Runtime uses it to pass to the linker, while Archive uses it
      -- to include the static runtime archives in the final archive object
   begin
      if GNARL then
         UBS.Append (Args, ' ' & RTS_Dir 
                       & (if Configuration.Position_Independent then
                             "/libgnarl_pic.a"
                          else
                             "/libgnarl.a"));
         
      end if;
      
      UBS.Append (Args, ' ' & RTS_DIR 
                    & (if Configuration.Position_Independent then
                          "/libgnat_pic.a"
                       else
                          "/libgnat.a"));
   end;
   
   ----------------------------------------------------------------------
   
   procedure Add_Runtime (Configuration: in     Build_Configuration;
                          Args         : in out UBS.Unbounded_String)
   is
      use UBS;
      
      RTS_Dir: constant String  := Find_Ada_RTS;
      GNARL  : constant Boolean := Needs_GNARL;
      -- Note in theory, there should be only one link per run of AURA, so it
      -- is perfectly fine to elaborate these locally, since it will only
      -- happen once anyways.
      
   begin
      
      if GNARL and then Platform_Info.Platform_Family = "unix" then
         Append (Args, " -pthread");
      end if;
      
      case Configuration.Linking is
         when Shared =>
            Append (Args, " -L" & RTS_Dir);
            Append (Args, " -lgnat");
            
            if GNARL then
               Append (Args, " -lgnarl");
            end if;
            
         when Static_RT | Static =>
            Add_Static_Runtime_Archives (Configuration, Args, GNARL, RTS_Dir);
      end case;
      
   end Add_Runtime;
   
   ----------------
   -- Link_Image --
   ----------------
   
   procedure Link_Image 
     (Image_Path   : in     String;
      Unit_Set     : in     Registrar.Library_Units.Library_Unit_Sets.Set;
      Configuration: in     Build_Configuration;
      Errors       :    out UBS.Unbounded_String)
   is
      use UBS;
      
      use Child_Processes;
      
      Args: Unbounded_String;

   begin
      -- Verify that we can find the Linker
      if not Program_Paths.Found (Binder) then
         raise Program_Error with
           "Link failed: Could not find the linker program (" 
           & Linker_Program & ").";
      end if;
      
      if Image_Path'Length = 0 then
         raise Constraint_Error with "Attempt to link without an image path";
      end if;
         
      Set_Unbounded_String (Args, "-o " & Image_Path);
      
      Add_Linker_Options (Configuration, Args);
      -- Add linker options
      
      -- Then the objects
      declare
         use Ada.Directories;
         use Registrar.Library_Units;
      begin
         
         for Unit of Unit_Set loop
            if Unit.Kind not in Unknown | Subunit then
               
               Append (Args, ' ' & Simple_Name (Object_File_Name (Unit)));
               -- We use Simple_Name and then execute the linker from the
               -- aura-build subdirectory to avoid any problems with
               -- overwhelming the arguments of the linker with long path-names
               -- for each object. It's also a bit nicer to look at when
               -- debugging
            end if;
         end loop;
      end;
      
      -- User and runtime libraries
      Add_User_Libraries (Args);
      Add_Runtime (Configuration, Args);
      
      -- Record the command
      declare
         use Ada.Text_IO;
         
         Path: constant String := Build_Output_Root & "/ada_main.linker.cmd";
         
         CMD_OUT: File_Type;
      begin
         if Ada.Directories.Exists (Path) then
            Open (File => CMD_OUT,
                  Mode => Out_File,
                  Name => Path);
         else
            Create (File => CMD_OUT,
                    Name => Path);
         end if;
         
         Put_Line (CMD_OUT, "Linker used:");
         Put_Line (CMD_OUT, Program_Paths.Image_Path (Linker));
         Put_Line (CMD_OUT, "Arguments used:");
         Put_Line (CMD_OUT, To_String (Args));
         Close (CMD_OUT);
      end;
      
      -- Execute
      
      declare
         use Child_Processes;
         
         Link_Process: Child_Process'Class
           := Spawn_Process
             (Image_Path        => Program_Paths.Image_Path (Linker),
              Arguments         => To_String (Args),
              Working_Directory => Build_Root);
         
         
         Discard  : Unbounded_String;
         Timed_Out: Boolean;
         Status   : Exit_Status;
      begin
         Wait_And_Buffer (Process   => Link_Process,
                          Poll_Rate => 0.1,
                          Timeout   => 300.0,
                          Output    => Discard,
                          Error     => Errors,
                          Timed_Out => Timed_Out,
                          Status    => Status);
         
         if Timed_Out then
            Link_Process.Kill;
             Append (Errors, " [TIMED OUT]");
         elsif Status = Failure and then Length (Errors) = 0 then
            Set_Unbounded_String (Errors, "[No error output]");
         end if;
      end;
      
   end Link_Image;
   
   -------------
   -- Archive --
   -------------
   
   procedure Archive 
     (Archive_Path : in     String;
      Unit_Set     : in     Registrar.Library_Units.Library_Unit_Sets.Set;
      Configuration: in     Build_Configuration;
      Errors       :    out UBS.Unbounded_String)
   is
      Args: UBS.Unbounded_String := UBS.To_Unbounded_String
        ("-rc " & Archive_Path & ' ');
      
      GNARL  : constant Boolean := Needs_GNARL;
      RTS_Dir: constant String  := Find_Ada_RTS;
      
      procedure Output_Linker_Options is
         use Ada.Strings.Fixed;
         use Ada.Text_IO;
         
         use all type Ada.Strings.Direction;
         

         Linker_Options: UBS.Unbounded_String;
         
         LO_File: File_Type;
         
         Extension_Start: constant Natural := Index (Source  => Archive_Path,
                                                     Pattern => ".",
                                                     Going   => Backward);
         
         Path: constant String 
           := Archive_Path (Archive_Path'First .. Extension_Start)
             & "linkopt";
         
      begin
         if Extension_Start < Archive_Path'First then
            -- This will likely be checked by the command processor,
            -- but this is such a cheap check to make, why not
            
            raise Constraint_Error with
              "Library archive path shall have an extension.";
         end if;
         
         if GNARL and then Platform_Info.Platform_Family = "unix" then
            UBS.Append (Linker_Options, " -pthread");
         end if;
         
         Add_Linker_Options (Configuration, Linker_Options);
         Add_User_Libraries (Linker_Options);
         Add_Runtime (Configuration, Linker_Options);
         
         if Ada.Directories.Exists (Path) then
            Open (File => LO_File,
                  Mode => Out_File,
                  Name => Path);
         else
            Create (File => LO_File,
                    Mode => Out_File,
                    Name => Path);
         end if;
         
         Put_Line (File => LO_File, Item => UBS.To_String (Linker_Options));
         Close (LO_File);
      end;
      
   begin
      pragma Assert (Configuration.Mode = Library);
      
      -- Verify that we can find the Archiver
      if not Program_Paths.Found (Archiver) then
         raise Program_Error with
           "Archive failed: Could not find the archiver program ("
           & Archiver_Program & ").";
      end if;
      

      
      declare
         use Ada.Directories;
         use Registrar.Library_Units;
      begin
         
         for Unit of Unit_Set loop
            if Unit.Kind not in Unknown | Subunit then
               
               UBS.Append (Args, ' ' & Simple_Name (Object_File_Name (Unit)));
               -- We use Simple_Name and then execute the linker from the
               -- aura-build subdirectory to avoid any problems with
               -- overwhelming the arguments of the linker with long path-names
               -- for each object. It's also a bit nicer to look at when
               -- debugging
            end if;
         end loop;
      end;
      
      -- Add the Ada runtime archives
      Add_Static_Runtime_Archives (Configuration, Args, GNARL, RTS_Dir);
      
      -- Record the command
      declare
         use Ada.Text_IO;
         
         Path: constant String := Build_Output_Root & "/ada_main.archiver.cmd";
         
         CMD_OUT: File_Type;
      begin
         if Ada.Directories.Exists (Path) then
            Open (File => CMD_OUT,
                  Mode => Out_File,
                  Name => Path);
         else
            Create (File => CMD_OUT,
                    Name => Path);
         end if;
         
         Put_Line (CMD_OUT, "Archiver used:");
         Put_Line (CMD_OUT, Program_Paths.Image_Path (Archiver));
         Put_Line (CMD_OUT, "Arguments used:");
         Put_Line (CMD_OUT, UBS.To_String (Args));
         Close (CMD_OUT);
      end;
      
      -- Execute
      
      declare
         use Child_Processes;
         
         Archive_Process: Child_Process'Class
           := Spawn_Process
             (Image_Path        => Program_Paths.Image_Path (Archiver),
              Arguments         => UBS.To_String (Args),
              Working_Directory => Build_Root);
         
         
         Discard  : UBS.Unbounded_String;
         Timed_Out: Boolean;
         Status   : Exit_Status;
      begin
         Wait_And_Buffer (Process   => Archive_Process,
                          Poll_Rate => 0.1,
                          Timeout   => 60.0,
                          Output    => Discard,
                          Error     => Errors,
                          Timed_Out => Timed_Out,
                          Status    => Status);
         
         if Timed_Out then
            Archive_Process.Kill;
            UBS.Append (Errors, " [TIMED OUT]");
            
         elsif Status = Success then

            Output_Linker_Options;
            
         elsif Status = Failure and then UBS.Length (Errors) = 0 then
            UBS.Set_Unbounded_String (Errors, "[No error output]");
         end if;
      end;
      
   end Archive;
   
   
   ---------------------
   -- Link_Subsystems --
   ---------------------
   
   procedure Link_Subsystems is
   begin
      -- TODO
      raise Program_Error with "Not implemented";
   end Link_Subsystems;
   
   
end Build.Linking;
