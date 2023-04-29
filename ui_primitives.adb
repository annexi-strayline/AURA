------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
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

with Ada.Calendar;
with Ada.Containers;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions;
with Ada.Strings.Fixed;

with CLI; use CLI;
with CLI.Widgets.Spinners;
with CLI.Widgets.Progress_Bars;

with Workers;
with Workers.Reporting;
with Registrar.Registration;
with Registrar.Queries;
with Registrar.Subsystems;
with Registrar.Library_Units;
with Registrar.Source_Files;
with Registrar.Implementation_Hashing;
with Repositories;
with Repositories.Cache;
with Progress;
with Unit_Names;
with User_Queries;
with User_Notices;
with Scheduling;

package body UI_Primitives is
   
   ------------------
   -- Print_Banner --
   ------------------
   
   procedure Print_Banner is
      subtype Banner_Line is String (1 .. 60);
      
      type Banner_Unit is array (Positive range <>) of Banner_Line;
      
      
      Banner: constant Banner_Unit(1 .. 4)
        := (1 => " ,====  == ==  ====.  ,====   Ada User Repository Annex     ",
            2 => ".==|==..==|==..==|==..==|==.  Reference Implementation      ",
            3 => ":-----::--|--::----.::-----:  Version 0.2                   ",
            4 => "|__|__||.___,||__|._||__|__|  (C) 2020-2023 ANNEXI-STRAYLINE");
      

      
      Banner_Color_Map: constant Banner_Unit(1 .. 4)
        := (1 => "511111551151155111115511111555555555555555555555555555555555",
            2 => "622622662262266226226622622666666666666666666666666666666666",
            3 => "733333773373377333337733333777777777777777777777777777777777",
            4 => "844844884444488448448844844888888888888888888888888888888888");

      
      type Style_Set is array (1 .. 8) of Text_Style;
      
      Styles: Style_Set := (1 => Blue_FG + Bold,
                            2 => Blue_FG + Bold,
                            3 => Blue_FG + Bold,
                            4 => Magenta_FG,
                            5 => White_FG,
                            6 => White_FG,
                            7 => White_FG,
                            8 => White_FG);
      
      
      function Map_To_Style (C: in Character) return Text_Style is
        (case C is
            when '1' => Styles(1),
            when '2' => Styles(2),
            when '3' => Styles(3),
            when '4' => Styles(4),
            when '5' => Styles(5),
            when '6' => Styles(6),
            when '7' => Styles(7),
            when others => Styles(8));
      

      
      Run_Start: Positive;
      Run_End  : Positive;
      Run_Sample: Character;
   begin
      Clear_Line;

      for L in Banner'Range loop
         
         declare
            Logo: Banner_Line renames Banner(L);
            Map : Banner_Line renames Banner_Color_Map(L);
         begin
            
            Run_Start := Map'First;
            
            while Run_Start <= Map'Last loop
               Run_Sample := Map(Run_Start);
               
               for I in Run_Start .. Map'Last loop
                  exit when Map(I) /= Run_Sample;
                  Run_End := I;
               end loop;
               
               Put (Message => Logo(Run_Start .. Run_End),
                    Style   => Map_To_Style (Run_Sample));
               
               
               Run_Start := Run_End + 1;
            end loop;
         end;
         
         New_Line;
      end loop;
      
      New_Line;
   end Print_Banner;
   
   ----------------
   -- Print_Help --
   ----------------
   
   procedure Print_Help is
   begin
      Put_Line (Message => "Usage: aura [command] [options]");
      New_Line;
      Put_Line ("If no command is specified, the command defaults to ""checkout""");
      New_Line;
      
      Put_Line (Message => "Available Commands", Style => Bold);
      New_Line;
      
      Put_Line (Message => "help", Style => Underline);
      New_Line;
      Put_Line (Message => "Display this message");
      New_Line;
      
      Put_Line (Message => "clean", Style => Underline);
      New_Line;
      Put_Line ("Removes all previous compilation objects, as well as all");
      Put_Line ("previous run data, including the build option history");
      New_Line;
      Put_Line ("This DOES NOT remove any compiled executables or libraries,");
      Put_Line ("however invoking aura build/library after aura clean will");
      Put_Line ("force their recompilation");
      New_Line;
      
      Put      (Message => "checkout", Style => Underline);
      Put_Line (Message => " [subsystem name] [[subsystem name] [...]]",
                Style   => Bold);
      New_Line;
      Put_Line ("Attempts to identify and check-out all required subsystems, ");
      Put_Line ("and also inducts those directly specified (including their ");
      Put_Line ("dependencies), but does not execute compilation.");
      New_Line;
      
      Put      (Message => "compile", Style => Underline);
      Put_Line (Message => " [-no-pic] [-debug] [-assertions] "
                  &        "[-optimize-1/2/3/size/debug]",
                Style   => Bold);
      New_Line;
      Put_Line ("-no-pic     Explicitly disable PIC compilation");
      Put_Line ("-debug      Enable debugging information. It is recommended");
      Put_Line ("            that -optimize-debug also be used");
      Put_Line ("-assertions Force all assertions on for all Ada units");
      Put_Line ("-optimize   Enable an optimization level. If not specified,");
      Put_Line ("            no optimization is applied.");
      Put_Line ("            -1/2/3: Optimization levels of increasing ");
      Put_Line ("                    optimization.");
      Put_Line ("            -size : Optimize for size");
      Put_Line ("            -debug: Optimize for debugging");
      New_Line;
      Put_Line ("The selected compile options are presistent. Subsequent");
      Put_Line ("invocations of aura compile with no other options causes");
      Put_Line ("the same options of the previous run to be used.");
      New_Line;
      Put_Line ("If no options were stored (after aura clean, or a new project,");
      Put_Line ("then the default options (no options) is used - which is to");
      Put_Line ("ouput unoptimized position-independent without debug info.");
      New_Line;
      
      Put      (Message => "build/run", Style => Underline);
      Put_Line (Message =>" [main unit] [-no-pie] [-static-rt/-static] [-debug]"
                  &       " [-assertions]",
                Style   => Bold);
      Put_Line (Message => "          [-optimize-1/2/3/size/debug]",
                Style   => Bold);
      New_Line;
      Put_Line ("Invokes checkout, then compile. If compilation succeeds, an");
      Put_Line ("executable is generated. If the command is run, and the");
      Put_Line ("build succeeds, the generated executable is executed.");
      New_Line;
      Put_Line ("The configuration invocation attempts to create a dynamically");
      Put_Line ("linked, position-independent executable (ASLR-capable).");
      New_Line;
      Put_Line ("-no-pie      Disables explicit pic compiler options during");
      Put_Line ("             compilation and pie linker options during");
      Put_Line ("             linking - implies the -no-pic compile option");
      Put_Line ("-static-rt   Attempts to statically link the Ada runtime");
      Put_Line ("             (and libgcc for GNAT) specifically");
      Put_Line ("-static      Attempts to force the executable to be fully");
      Put_Line ("             statically linked (including libc, pthreads,");
      Put_Line ("             etc.) Only use this option if you know what");
      Put_Line ("             you are doing. This option implies -no-pic");
      Put_Line ("             and -static-rt");
      New_Line;
      Put_Line ("All other parameters are passed to compile. Like compile,");
      Put_Line ("these options are persistent between runs.");
      New_Line;
      Put_Line ("If a main unit is not given, the resulting executable");
      Put_Line ("elaborates all root units on execution.");
      New_Line;
      Put_Line ("If a static executable is built, the required libraries for");
      Put_Line ("linking must be at a path specified by the environment variable");
      Put_Line ("LIBRARY_PATH, and must have the format ""library_name.a"".");
      New_Line;
      Put_Line ("The executable generated takes the name of the main unit, if");
      Put_Line ("specified. If no main unit is specified, the executable takes ");
      Put_Line ("the name of aura.out");
      New_Line;
      
      Put      (Message => "library", Style => Underline);
      Put_Line (Message => " [-no-pie] [-static-rt/-static] [-debug] "
                  & "[-optimize-1/2/3/size/debug]",
                Style   => Bold);
      Put_Line (Message => "        library_name.a/so/dylib/dll",
                Style   => Bold);
      New_Line;
      Put_Line ("[-optimize-1/2/3/size/debug] library_name.a/so/dylib/dll");
      New_Line;
      Put_Line ("Used to generate a stand-alone library that may be included in");
      Put_Line ("other non-ada programs.");
      New_Line;
      Put_Line ("To create Ada-specific shared libraries, see the systemize command");
      New_Line;
      Put_Line ("Library Invokes fetch then compile. Before linking or archiving the");
      Put_Line ("specified library");
      New_Line;
      Put_Line ("The created library does not have a main subprogram, but will have");
      Put_Line ("symbols for C-exported adainit and adafinal C-convention subprograms.");
      Put_Line ("These subprograms must be invoked before and after using the library,");
      Put_Line ("Respectively");
      New_Line;
      Put_Line ("NOTE: Using multiple aura-built libraries in a single executable is not");
      Put_Line ("possible due to the initialization/finalization subprograms. However a");
      Put_Line ("feature to allow specific naming of those subprograms may be added in the");
      Put_Line ("future.");
      New_Line;
      Put_Line ("The library type (static vs dynamic) is determined by the extension");
      New_Line;
      Put_Line ("If a static library (archive) is built, the required libraries for");
      Put_Line ("linking must be either in the project root, or else must");
      Put_Line ("be at a path specified by the environment variable");
      Put_Line ("LIBRARY_PATH, and must have the format ""library_name.a"".");
      New_Line;
      Put_Line ("For static libraries (archives), -static is implied. This, in-");
      Put_Line ("effect causes a static Ada runtime to be included in the archive");
      New_Line;
      Put_Line ("Additionally, when building static libraries, all Linker_Options");
      Put_Line ("pragmas, together with required libraries will be output into a");
      Put_Line ("separate file ""library_name.linkopt"". These options should be passed");
      Put_Line ("to the linker or linker driver when using the static library");
      New_Line;
      Put_Line ("For dynamic libraries, if -static or -static-rt is NOT given, the");
      Put_Line ("resulting library will have dynamic dependencies on the shared Ada");
      Put_Line ("runtime. Care should be taken when using partition-specific pragmas");
      Put_Line ("accross multiple such libraries, as they will have global effects");
      Put_Line ("due to a single Ada runtime being shared amongst them.");
      New_Line;
      Put_Line ("Using -static or -static-rt when building a shared library will cause");
      Put_Line ("the resulting library to contian it's own instance of the Ada runtime");
      New_Line;

      New_Line;
      Put_Line ("If the "".so"" extension is given for library_name, then");
      Put_Line ("even if ""-static"" is given, the produced library will be");
      Put_Line ("a dynamic library.");
      New_Line;
      Put_Line ("The initialization (elaboration) and finalization are");
      Put_Line ("registered with the linker for shared (default) or");
      Put_Line ("-static-rt libraries. If -static is used, the adainit and");
      Put_Line ("adafinal must be called by the user of the library");
      New_Line;
      Put_Line ("Options -static/-static-rt have no effect if buidling an");
      Put_Line ("archive library");
      New_Line;

      New_Line;
      
      Put      (Message => "systemize", Style => Underline);
      Put_Line (Message => " [-repo-add/show] [-debug] [-optmize-1/2/3/size/debug]",
                Style   => Bold);
      Put_Line (Message => "          destination_path",
                Style   => Bold);
      New_Line;
      Put_Line ("Systemize builds separate shared libraries for each AURA");
      Put_Line ("subsystem, and then installs them into a read-only pre-");
      Put_Line ("formatted ""System"" AURA Repository at destination_path");
      New_Line;
      Put_Line ("-repo-add   A repository that points to the newly created");
      Put_Line ("            System Repository is added to the project");
      New_Line;
      Put_Line ("-repo-show  The content needed to create a Repository Spec");
      Put_Line ("            for the newly created System Repository is output.");
      Put_Line ("            This is the default.");
      
      New_Line;
      
      Put_Line (Message => "General Options", Style => Underline);
      Put_Line ("-v  When common errors occur, output extra information");
      Put_Line ("-q  When common errors occur, output minimum information");
      Put_Line ("-y  Answer all queries automatically with the default response");
      New_Line;
      
   end Print_Help;
   
   -------------
   -- Put_Tag --
   -------------
   
   procedure Put_Tag (Tag: Tag_Type) is
      Tag_Styles: constant array (Tag_Type) of Text_Style
        := (EXEC  => Black_BG    + Cyan_FG + Bold,
            OK    => Green_FG    + Bold,
            FAIL  => Red_BG      + White_FG,
            WARN  => Yellow_BG   + Black_FG,
            QUERY => Magenta_BG  + White_FG,
            INFO  => Blue_BG     + White_FG);
      
      subtype Tag_String is String (1 .. 6);
      Term_Tags: constant array (Tag_Type) of Tag_String
        := (EXEC  => " EXEC ",
            OK    => "  OK  ",
            FAIL  => " FAIL ",
            WARN  => " WARN ",
            QUERY => " QURY ",
            INFO  => " INFO ");
      
      
      Pipe_Tags: constant array (Tag_Type) of Tag_String
        := (EXEC  => "[EXEC]",
            OK    => "[OKAY]",
            FAIL  => "[FAIL]",
            WARN  => "[WARN]",
            QUERY => "[QURY]",
            INFO  => "[INFO]");
   begin
      if Output_Is_Terminal then
         Put (Message => Term_Tags (Tag), Style => Tag_Styles (Tag));
      else
         Put (Pipe_Tags(Tag));
      end if;
   end Put_Tag;
   
   -------------------
   -- Put_Empty_Tag --
   -------------------
   
   procedure Put_Empty_Tag is
   begin
      Put (String'(1 .. 6 => ' '));
   end Put_Empty_Tag;
   
   -----------------
   -- Put_Divider --
   -----------------
   
   procedure Put_Divider is
   begin
      Put_Line (String'(1 .. Terminal_Width => '-'));
   end Put_Divider;
   
   ------------------
   -- Query_Driver --
   ------------------
   
   procedure Query_Driver (Prompt  : in     String;
                           Default : in     String;
                           Response:    out String;
                           Last    :    out Natural)
   is
      use Ada.Characters.Latin_1;
   begin
      Clear_Line;
      Put_Query_Tag;
      Put (' ' & Prompt);
      
      if Auto_Queries then
         Put_Line (Default & " [AUTO-ANSWER DEFAULT]");
         Response := Default;
         Last := Default'Last;
         
      elsif not CLI.Output_Is_Terminal then
         New_Line;
         Put_Fail_Tag;
         Put_Line (" Query response required while running headless.");
         Put_Empty_Tag;
         Put_Line (" Re-run on a terminal, or else give the '-y' option to "
                     & "select the default option for all queries.");
         Put_Empty_Tag;
         Put_Line (" Aborting.");
         raise Scheduling.Process_Failed with
           "No one to answer the Query.";
      else
         Get_Line (Item => Response,
                   Last => Last);
      end if;
   end;
   
   
   ------------------------
   -- Immediate_YN_Query --
   ------------------------
   
   procedure Immediate_YN_Query (Prompt  : in     String;
                                 Default : in     Boolean;
                                 Response:    out Boolean)
   is
      Query_Active: Boolean := False;
      Query_Response: String (1 .. 1);
      Last: Natural;
   begin
      loop
         select
            User_Queries.Query_Manager.Start_Query;
            Query_Active := True;
         else
            raise Program_Error with "Unexpected: user query active";
         end select;
         
         User_Queries.Query_Manager.Post_Query
           (Prompt        => Prompt & " (y/n)",
            Default       => (if Default then "y" else "n"),
            Response_Size => 1);
         
         User_Queries.Query_Manager.Take_Query
           (UI_Primitives.Query_Driver'Access);
         

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
                  Response := True;
                  exit;
                  
               when 'n' | 'N' =>
                  Response := False;
                  exit;
                  
               when others =>
                  Put_Info_Tag;
                  Put_Line (" You must answer y or n");
                  CLI.New_Line;
            end case;
         end if;
      end loop;
      
   exception
      when others =>
         if Query_Active then
            User_Queries.Query_Manager.End_Query;
         end if;
         raise;
   end Immediate_YN_Query;
   
   ------------------
   -- Dump_Reports --
   ------------------
   
   procedure Dump_Reports is
      use Workers, Workers.Reporting;
      
      R: Work_Report;
   begin
      for I in 1 .. Available_Reports loop
         R := Retrieve_Report;
         
         Put_Line ("-- Worker Report" & Count_Type'Image (I)
                     & " (" & Report_Kind'Image (R.Kind) & ')'
                     & " --");
         Put_Line (To_String (R.Work_Order_Information));
         
         if R.Kind = Error then
            CLI.New_Line;
            Put_Line (To_String (R.Exception_Information));
         end if;
         
         if Length (R.Worker_Note) > 0 then
            CLI.New_Line;
            Put_Line ("Worker Note: " & To_String (R.Worker_Note));
         end if;
         
         CLI.New_Line;
      end loop;
   end Dump_Reports;
   
   -----------------------
   -- Dump_Repositories --
   -----------------------
   
   procedure Dump_Repositories is
      use Repositories;
      
      All_Repos: constant Repository_Maps.Map := Extract_All_Repositories;
      
      I: Repository_Index := Repository_Index'First;
   begin
      New_Line;
      Put_Line ("Repositories");
      
      for Repo of All_Repos loop
         New_Line;
         Put_Line ("Repository" & Repository_Index'Image (I));
         Put_Line (" Format         : " 
                     & Repository_Format'Image (Repo.Format));
         Put_Line (" Location       : " & UBS.To_String (Repo.Location));
         Put_Line (" Snapshot       : " & UBS.To_String (Repo.Snapshot));
         Put_Line (" Cache_State    : " 
                     & Repository_Cache_State'Image (Repo.Cache_State));
         Put_Line (" Cache_Path     : " & UBS.To_String (Repo.Cache_Path));
         
         if Repo.Format = Git then
            Put_Line (" Tracking_Branch: " 
                        & UBS.To_String (Repo.Tracking_Branch));
         end if;
         
         I := I + 1;
      end loop;
   end Dump_Repositories;
   
   ---------------------
   -- Dump_Subsystems --
   ---------------------
   
   procedure Dump_Subsystems is
      use Ada.Containers;
      use Registrar.Queries;
      use Registrar.Subsystems;
      use Registrar.Source_Files;
      use Unit_Names;
      
      Subsystems: Subsystem_Sets.Set := All_Subsystems;
   begin
      for SS of Subsystems loop
         New_Line;
         Put_Line (SS.Name.To_UTF8_String);
         Put_Line (" AURA       : " & Boolean'Image (SS.AURA));
         Put_Line (" State      : " & Subsystem_State'Image (SS.State));
         
         if SS.State = Available and then SS.AURA then
            Put_Line ("--- Config ---");
            Put_Line (" External_Libraries:");
            for Item of SS.Configuration.External_Libraries loop
               Put ("  " & Ada.Characters.Conversions
                      .To_String (WWU.To_Wide_Wide_String (Item.Name)));
               Put_Line (" => " & UBS.To_String (Item.Value));
            end loop;
            
            Put_Line (" Ada_Compiler_Opts");
            for Item of SS.Configuration.Ada_Compiler_Opts loop
               Put ("  " & Ada.Characters.Conversions
                      .To_String (WWU.To_Wide_Wide_String (Item.Name)));
               Put_Line (" => " & UBS.To_String (Item.Value));
            end loop;
            
            Put_Line (" C_Compiler_Opts");
            for Item of SS.Configuration.C_Compiler_Opts loop
               Put ("  " & Ada.Characters.Conversions
                      .To_String (WWU.To_Wide_Wide_String (Item.Name)));
               Put_Line (" => " & UBS.To_String (Item.Value));
            end loop;
            
            Put_Line (" C_Definitions");
            for Item of SS.Configuration.C_Definitions loop
               Put ("  " & Ada.Characters.Conversions
                      .To_String (WWU.To_Wide_Wide_String (Item.Name)));
               Put_Line (" => " & UBS.To_String (Item.Value));
            end loop;
            
            Put_Line (" Codepaths");
            for Item of SS.Configuration.Codepaths loop
               Put ("  " & Ada.Characters.Conversions
                      .To_String (WWU.To_Wide_Wide_String (Item.Name)));
               Put_Line (" => " & UBS.To_String (Item.Value));
            end loop;
            
            Put_Line (" Information");
            for Item of SS.Configuration.Information loop
               Put ("  " & Ada.Characters.Conversions
                      .To_String (WWU.To_Wide_Wide_String (Item.Name)));
               Put_Line (" => " & UBS.To_String (Item.Value));
            end loop;
            
         end if;
         
      end loop;
      
   end Dump_Subsystems;
   
   ------------------------
   -- Dump_Library_Units --
   ------------------------
   
   procedure Dump_Library_Units is
      use Ada.Containers;
      use Registrar.Queries;
      use Registrar.Library_Units;
      use Registrar.Source_Files;
      use Unit_Names;
      
      Units: Library_Unit_Sets.Set := All_Library_Units;
   begin
      for Unit of Units loop
         New_Line;
         Put_Line (Unit.Name.To_UTF8_String);
         Put_Line (" Subsystem  : " & Unit.Name.Subsystem_Name.To_UTF8_String);
         Put_Line (" State      : " & Library_Unit_State'Image (Unit.State));
         Put_Line (" Kind       : " & Library_Unit_Kind'Image (Unit.Kind));
         Put_Line (" Is_Generic : " & Boolean'Image (Unit.Is_Generic));
         Put_Line (" Have Spec? : " & Boolean'Image (Unit.Spec_File /= null));
         Put_Line (" Have Body? : " & Boolean'Image (Unit.Body_File /= null));
         Put_Line (" Subunits   : " & Count_Type'Image (Unit.Subunit_Bodies.Length));
         Put_Line (" Spec Hash  : " & Unit.Specification_Hash.To_String);
         Put_Line (" Impl Hash  : " & Unit.Implementation_Hash.To_String);
         
         Put_Line (" Spec_File  : " & (if Unit.Spec_File /= null then Unit.Spec_File.Hash.To_String
                                       else ""));
         Put_Line (" Body_File  : " & (if Unit.Body_File /= null then Unit.Body_File.Hash.To_String
                                       else ""));
      end loop;
   end Dump_Library_Units;
   
   --
   -- Trackers
   --
   
   Process_Title_Style: constant Text_Style := Blue_FG + Bold;
   
   Progress_Bar_Template: constant CLI.Widgets.Progress_Bars.Progress_Bar 
     := (Delimited   => True,
         Width       => 10,
         Fill_Char   => ' ',
         Fill_Style  => White_BG,
         Empty_Char  => ' ',
         Empty_Style => Neutral,
         others      => <>);
   
   ---------------------------
   -- Internal_Prep_Tracker --
   ---------------------------
   
   procedure Internal_Prep_Tracker
     (Process_Title: in     String;
      Bar          : in out CLI.Widgets.Progress_Bars.Progress_Bar;
      Spinner_Only : in     Boolean)
   is
      use CLI.Widgets.Progress_Bars;
   begin
      Clear_Line;
      Put_Exec_Tag;
      Put (' ');
      Put (Message => Process_Title, Style => Process_Title_Style);
      Put (' ');
      
      if not Spinner_Only then
         Render (Bar);
      end if;
   end Internal_Prep_Tracker;
   
   ------------------
   -- Prep_Tracker --
   ------------------
   
   procedure Prep_Tracker (Process_Title: in String;
                           Spinner_Only : in Boolean := False) is
      use CLI.Widgets.Progress_Bars;
      
      Zeroed_Bar: Progress_Bar := Progress_Bar_Template;
   begin
      if not Output_Is_Terminal then return; end if;
      Internal_Prep_Tracker (Process_Title => Process_Title,
                             Bar           => Zeroed_Bar,
                             Spinner_Only  => Spinner_Only);
   end Prep_Tracker;
   
   ------------------
   -- Wait_Tracker --
   ------------------
   
   procedure Wait_Tracker (Process_Title  : in     String;
                           Tracker        : in out Progress.Progress_Tracker;
                           Failures       :    out Boolean;
                           Timedout       :    out Boolean;
                           Spinner_Only   : in     Boolean := False;
                           Process_Timeout: in     Duration := 60.0)
   is
      use CLI.Widgets.Progress_Bars;
      use CLI.Widgets.Spinners;
      use type Ada.Calendar.Time;
      use type Ada.Containers.Count_Type;
      
      function Trim (Source: in String; 
                     Side: in Ada.Strings.Trim_End := Ada.Strings.Both)
                    return String
        renames Ada.Strings.Fixed.Trim;
      
      Successful_Items_Style: Text_Style renames Neutral;
      Failed_Items_Style    : Text_Style renames Red_FG;
      Total_Items_Style     : Text_Style renames Neutral;
      Timeout_Label_Style   : constant Text_Style := Yellow_BG + Black_FG;
      
      Bar      : Progress_Bar := Progress_Bar_Template;
      Spin     : Spinner;
      After_Bar: Positive;
      Is_Term  : constant Boolean  := Output_Is_Terminal;
      
      Deadline: constant Ada.Calendar.Time
        := Ada.Calendar.Clock + Process_Timeout;
      
      Total_Items    : Natural;
      Completed_Items: Natural;
      Failed_Items   : Natural;
      
      procedure Get_Totals is
      begin
         Total_Items     := Tracker.Total_Items;
         Completed_Items := Tracker.Completed_Items;
         Failed_Items    := Tracker.Failed_Items;
      end Get_Totals;
      
      procedure Prep_Output is
      begin
         if Is_Term then
            Internal_Prep_Tracker (Process_Title, Bar, Spinner_Only);
            Put (' ');
            Render (Spin);
            Put (' ');
            After_Bar := Current_Column;
         end if;
      end Prep_Output;
      
      procedure Term_Update is
      begin
         Get_Totals;
         
         if not Spinner_Only then
            Bar.Percent := Percentage (Tracker.Percent_Complete);
            
            if not Failures and then Failed_Items > 0 then
               Failures := True;
               Set_Column (1);
               Put_Fail_Tag;
            end if;
               
            Update (Bar);
            Set_Column (After_Bar);
            Clear_To_End;
            Put (Message => Trim (Natural'Image (Completed_Items)),
                 Style   => Successful_Items_Style);
            
            if Failures then
               Put (Message => " (+" & Trim (Natural'Image (Failed_Items))
                      &        " Failed)",
                       Style => Failed_Items_Style);
            end if;
            
            Put (" of ");
            Put (Message => Trim (Natural'Image (Total_Items)),
                 Style => Total_Items_Style);
            Put (" work orders.");
            
            if Timedout then
               Put (Message => " * TIMEOUT *",
                    Style   => Timeout_Label_Style);
            end if;
         end if;
         
         Update (Spin);
      end Term_Update;
      
      procedure Post_Notices is
          use User_Notices;
      begin
         while User_Notices.Available_Notices > 0 loop
            declare
               Notice: constant Notice_Lines := Retrieve_Notice;
            begin
               if Is_Term then
                  Clear_Line;
               else
                  New_Line;
               end if;
               Put_Info_Tag;
               Put (' ');
               for Line of Notice loop
                  Put_Line (UBS.To_String (Line));
                  Put_Empty_Tag;
                  Put (' ');
               end loop;
            end;
            
            Clear_Line;
            Prep_Output;
         end loop;
      end Post_Notices;
      
   begin
      Failures := False;
      Timedout := False;
      
      if Is_Term then
         Prep_Output;
         null;
      else
         New_Line;
         Put_Exec_Tag;
         Put (' ' & Process_Title & " ...");
      end if;
      
      if Is_Term then
         loop
            Term_Update;
            
            exit when Tracker.Is_Complete;
            
            if Ada.Calendar.Clock > Deadline then
               Set_Column (1);
               Put_Fail_Tag;
               Timedout := True;
               Term_Update;
               return;
            else
               select
                  Tracker.Wait_Complete;
               or
                  delay Progress_Poll_Rate;
               end select;
            end if;
            
            Post_Notices;
            
            if User_Queries.Query_Manager.Query_Pending then
               User_Queries.Query_Manager.Take_Query (Query_Driver'Access);
               Prep_Output;
            end if;
         end loop;
         
         Post_Notices;
         
      else
         
         loop
            select
               Tracker.Wait_Complete;
               exit;
            or
               delay Progress_Poll_Rate;
            end select;
            
            Post_Notices;
            
            if Ada.Calendar.Clock > Deadline then
               Timedout := True;
               exit;
            
            elsif User_Queries.Query_Manager.Query_Pending then
               User_Queries.Query_Manager.Take_Query (Query_Driver'Access);
               
            end if;
         end loop;
         
         Post_Notices;
         
         Get_Totals;
         Failures := (Failed_Items > 0);
         
         New_Line;
         
         if Timedout then
            Put_Fail_Tag;
            Put (" * TIMEOUT *");
         elsif Failures then
            Put_Fail_Tag;
         else
            Put_OK_Tag;
         end if;
         
         Put (Natural'Image (Completed_Items));
         
         if Failures then
            Put (" (+" & Trim (Natural'Image (Failed_Items)) & " Failed)");
         end if;
         
         Put_Line (" of" 
                     & Natural'Image (Total_Items) 
                     & " work orders completed.");
      end if;
      
   end Wait_Tracker;
   
   ---------------------------
   -- Wait_Tracker_Or_Abort --
   ---------------------------
   
   procedure Wait_Tracker_Or_Abort 
     (Process_Title  : in     String;
      Tracker        : in out Progress.Progress_Tracker;
      Spinner_Only   : in     Boolean := False;
      Process_Timeout: in     Duration := 20.0)
   is
      Failures, Timedout: Boolean := False;
      
   begin
      Wait_Tracker (Process_Title   => Process_Title,
                    Tracker         => Tracker,
                    Failures        => Failures,
                    Timedout        => Timedout,
                    Process_Timeout => Process_Timeout);
      
      if Failures or else Timedout then
         New_Line;
         raise Scheduling.Process_Failed;
      end if;
      
   end Wait_Tracker_Or_Abort;
   
   --------------
   -- Put_Info --
   --------------
   
   procedure Put_Info (Message: String) is
   begin
      case Output_Is_Terminal is
         when True  => Put (Message);
         when False => Put_Line (Message);
      end case;
   end;
   
end UI_Primitives;
