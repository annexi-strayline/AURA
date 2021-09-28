------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2021, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Platform_Info;

separate (Scheduling)

procedure Initialize_Parameters is
   use UBS;
   
   use all type Build.Build_Mode;
   use all type Build.Link_Mode;
   use all type Build.Compile_Optimization_Mode;
   
   Arg_Count: constant Natural := Ada.Command_Line.Argument_Count;
   Options_Start: Positive;
   -- Indicates the index into the Arguments vector containing the first
   -- option
   
   function Argument (Number: in Positive) return String
     renames Ada.Command_Line.Argument;
   
   Arg: Unbounded_String;
   
   procedure Unrecognized_Option is
   begin
      UI.Put_Fail_Tag;
      Put_Line (" Unrecognized option """ & To_String (Arg) & """.");
      Command := Help_Command;
   end Unrecognized_Option;
   
   function Is_Option (Arg_S: in String) return Boolean is
      (Ada.Strings.Fixed.Head (Arg_S, 1) = "-");
   
   procedure Check_Option_Start (Arg_S   : in     String;
                                 Expected: in     String;
                                 After   :    out Positive)
   is
      -- If Expected matches the beginning of Arg, then After is the index into
      -- Arg which is the first character after Expected. Otherwise, consider
      -- the option to be invalid
   begin
      After := Arg_S'First + Expected'Length;
      
      if Arg_S'Length <= Expected'Length 
         -- This procuedure is only used for cases where an option has varients,
         -- such as -optimize-1 vs -optimize-2, so it must at least be longer
         -- than the expected
        or else Arg_S(Arg_S'First .. After - 1) /= Expected
      then
         Unrecognized_Option;
      end if;
   end Check_Option_Start;
   
   
begin
   Build.Load_Last_Build_Config (Parameters.Build_Config);
   -- Load last configuration (if available)
   
   -- Collect the command, if any
   
   if Arg_Count = 0 then
      -- Use the default command
      return;
   end if;
   
   if not Is_Option (Argument(1)) then
      declare
         package ACH renames Ada.Characters.Handling;
         Command_Text: constant String := ACH.To_Lower (Argument (1));
         Bad_Command : Boolean := True;
         
         procedure Check_Command (Expected: in Selected_Command;
                                  Check   : in String) 
         is begin
            if Command_Text = Check then
               Command := Expected;
               Bad_Command := False;
            else
               Bad_Command := True;
            end if;
         end;
            
      begin
         if Command_Text'Length < 2 then
            Bad_Command := True;
            
         else
            case Command_Text(Command_Text'First) is
               when 'b' => Check_Command (Build_Command,     "build"    );
                  
               when 'c' => 
                  case Command_Text(Command_Text'First + 1) is
                     when 'h' => Check_Command (Checkout_Command, "checkout" );
                     when 'l' => Check_Command (Clean_Command,    "clean"    );
                     when 'o' => Check_Command (Compile_Command,  "compile"  );
                        
                     when others => Bad_Command := True;
                  end case;
                        
               when 'h' => Check_Command (Help_Command,      "help"     );
               when 'l' => Check_Command (Library_Command,   "library"  );
               when 'r' => Check_Command (Run_Command,       "run"      );
               when 's' => Check_Command (Systemize_Command, "systemize");
                  
               when others => Bad_Command := True;
            end case;
         end if;
         
         if Bad_Command then
            UI.Put_Fail_Tag;
            Put_Line (" Command """ & Command_Text & """ not recognized.");
            Command := Help_Command;
            return;
         end if;
         
      end;
      
      Options_Start := 2;
      
   else
      Options_Start := 1;
      
   end if;
   
   
   -- Capture the "main unit" if it is defined (only for the relevant commands)
   if Command in Build_Command | Run_Command
     and then Options_Start <= Arg_Count
     and then not Is_Option (Argument(Options_Start))
   then
      -- We have to assume that this unit name is utf-8 encoded
      declare
         package UTF   renames Ada.Strings.UTF_Encoding;
         package WWUTF renames UTF.Wide_Wide_Strings;
         
         WWName: constant Wide_Wide_String
           := WWUTF.Decode (UTF.UTF_8_String'(Argument(Options_Start)));
         
      begin
         
         if Unit_Names.Valid_Unit_Name (WWName) then
            Parameters.Main_Unit := Unit_Names.Set_Name (WWName);
            Options_Start := Options_Start + 1;
            
            if Parameters.Main_Unit.Is_External_Unit then
               UI.Put_Fail_Tag;
               Put_Line (" External main units are not supported.");
               raise Process_Failed;
            end if;
         else
            UI.Put_Fail_Tag;
            Put_Line (" Main Unit Name """ & Argument (Options_Start)
                        & """ is not a valid Ada unit name.");
            raise Process_Failed;
         end if;
      end;
   end if;
   
   
   -- Process all options, and check against the command
   Parameters.Last_Argument := Options_Start;
   
     Option_Iteration:
   for I in Options_Start .. Arg_Count loop
      Set_Unbounded_String 
        (Target => Arg,
         Source => Ada.Characters.Handling.To_Lower (Argument (I)));
      
      if Element (Arg, 1) /= '-' then 
         -- End of the options
         
         Parameters.Last_Argument := I - 1;
         exit;
      end if;
      
      
      case Element (Arg, 2) is
         when 'a' =>
            if To_String (Arg) = "-assertions" then
               Parameters.Build_Config.All_Assertions := True;
               
            else
               Unrecognized_Option;
               return;
            end if;
            
         when 'd' =>
            if To_String (Arg) = "-debug" then
               Parameters.Build_Config.Debug_Enabled := True;
            else
               Unrecognized_Option;
               return;
            end if;
            
            
         when 'n' =>
            -- Must be -no-pie or -no-pic
            if Arg = "-no-pic" or else Arg = "-no-pie"
            then
               Parameters.Build_Config.Position_Independent := False;
            else
               Unrecognized_Option;
               return;
            end if;
            
         when 'o' =>
            -- Optimize of some kind
            declare
               Arg_S: constant String := To_String (Arg);
               Diff_Start: Positive;
            begin
               Check_Option_Start (Arg_S    => Arg_S,
                                   Expected => "-optimize-",
                                   After    => Diff_Start);
               
               case Arg_S (Diff_Start) is
                  when '1' => Parameters.Build_Config.Optimization := Level_1;
                  when '2' => Parameters.Build_Config.Optimization := Level_2;
                  when '3' => Parameters.Build_Config.Optimization := Level_3;
                  when 's' => Parameters.Build_Config.Optimization := Size;
                  when 'd' => Parameters.Build_Config.Optimization := Debug;
                     
                  when others => 
                     Unrecognized_Option;
                     return;
               end case;                  
            end;
            
            
         when 'q' =>
            if Arg = "-q" then
               Parameters.Output_Style := Quiet;
            else
               Unrecognized_Option;
               return;
            end if;
            
         when 'r' =>
            declare
               Arg_S: constant String := To_String (Arg);
               Diff_Start: Positive;
            begin
               Check_Option_Start (Arg_S    => Arg_S,
                                   Expected => "-repo-",
                                   After    => Diff_Start);
               
               case Arg_S (Diff_Start) is
                  when 'a' => Parameters.Systemize_Mode := Add;
                  when 's' => Parameters.Systemize_Mode := Show;
                     
                  when others =>
                     Unrecognized_Option;
                     return;
               end case;
               
            end;
            
         when 's' =>
            if Arg = "-static" then
               Parameters.Build_Config.Linking := Static;
               Parameters.Build_Config.Position_Independent := False;
            elsif Arg = "-static-rt" then
               Parameters.Build_Config.Linking := Static_RT;
            else
               Unrecognized_Option;
               return;
            end if;
            
         when 'v' =>
            if Arg = "-v" then
               Parameters.Output_Style := Verbose;
            else
               Unrecognized_Option;
               return;
            end if;
            
         when 'y' =>
            if Arg = "-y" then
               UI_Primitives.Auto_Queries := True;
            else
               Unrecognized_Option;
               return;
            end if;
            
         when others =>
            Unrecognized_Option;
            return;
      end case;
   end loop Option_Iteration;
   
   
   -- Check library name validity, and determine if it is a static (archive) or
   -- dynamic library. We can also alert the user of an override of -static if
   -- the library is to be an archive.
   --
   -- After this, the actual Link_Or_Archive phase can be confident the library
   -- name is valid and appropriate for the platform and configuration
   
   if Command = Library_Command then
      if Parameters.Last_Argument = Arg_Count then
         UI.Put_Fail_Tag;
         Put_Line (" Library builds must include a target library output "
                     & "filename.");
         raise Process_Failed;
      end if;
      
      -- Extract the extension slice of the expected file name
      declare
         Output_Name: constant String 
           := Argument(Parameters.Last_Argument + 1);
         
         Extension: constant String
           := Ada.Characters.Handling.To_Lower 
             (Ada.Directories.Extension 
                (Argument(Parameters.Last_Argument + 1)));
      begin
         
         if Extension = "a" then
            if Parameters.Build_Config.Linking /= Static then
               UI.Put_Info_Tag;
               Put_Line (" Archive libraries (.a) imply -static.");
               Parameters.Build_Config.Linking := Static;
            end if;
            
         elsif Extension = "so" then
            if Platform_Info.Platform_Family = "windows" then
               UI.Put_Fail_Tag;
               Put_Line (" Windows only supports .a and .dll libraries.");
               raise Process_Failed;
               
            elsif Platform_Info.Platform_Family = "unix"
              and then Platform_Info.Platform_Flavor = "darwin"
            then
               UI.Put_Fail_Tag;
               Put_Line (" Darwin only supports .a and .dylib "
                           & "libraries.");
               raise Process_Failed;
            end if;
            
         elsif Extension = "dylib" then
            if Platform_Info.Platform_Family /= "unix"
              or else Platform_Info.Platform_Flavor /= "darwin"
            then
               UI.Put_Fail_Tag;
               Put_Line (" .dylib libraries are only supported for Darwin "
                           & "targets.");
               raise Process_Failed;
            end if;
            
         elsif Extension = "dll" then
            if Platform_Info.Platform_Family /= "windows" then
               UI.Put_Fail_Tag;
               Put_Line (" .dll libraries are only supported for Windows "
                           & "targets.");
               raise Process_Failed;
            end if;
            
         else
            UI.Put_Fail_Tag;
            Put_Line (" Library name must end in a valid extension " 
                        & "(.a/.so/.dylib/.dll)");
            
            raise Process_Failed;
         end if;
      end;
   end if;
   
   -- Systemize is tightly controlled by AURA, so we need to warn the user
   -- when they attempt to use invalid options
   
   if Command = Systemize_Command 
     and then (not Parameters.Build_Config.Position_Independent
                 or else Parameters.Build_Config.Linking /= Shared)
   then
      UI.Put_Warn_Tag;
      Put_Line (" -no-pie/-no-pic and -static/-static-rt are not relevant when");
      Put_Line (" building a System repository.");
      
      Parameters.Build_Config.Position_Independent := True;
      Parameters.Build_Config.Linking              := Shared;
      raise Process_Failed;
   end if;
   
   
   -- Finally set the Build mode
   case Command is
      when Build_Command | Run_Command =>
         Parameters.Build_Config.Mode := Image;
         
      when Library_Command =>
         Parameters.Build_Config.Mode := Library;
         
      when Systemize_Command =>
         Parameters.Build_Config.Mode := Systemize;
         
      when Checkout_Command | Clean_Command | Compile_Command | Help_Command =>
         -- We simply set an arbitrary default in this case, as the build mode
         -- won't actually matter in these cases
         Parameters.Build_Config.Mode := Image;
   end case;
   
end Initialize_Parameters;
