------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
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

with Ada.Strings;
with Ada.Strings.Unbounded;     
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Environment_Variables;
with Ada.Directories;

package body Child_Processes.Path_Searching is
   
   procedure Not_Found_Error (Program: String) with Inline, No_Return is
   begin
      raise Not_In_Path with
        "Program """ & Program & """ was not found in PATH.";
   end;

   
   -----------------
   -- Search_Path --
   -----------------
   
   function Search_Path (Program: String) return String is
      use Ada.Strings.Unbounded;
      package ENV renames Ada.Environment_Variables;
      package Path_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive,
         Element_Type => Unbounded_String);
      
      Path_List: Path_Vectors.Vector;
      
   begin
      
      -- Parse the PATH environment varaible - a colon-separated list
      declare
         use Ada.Strings;
         
         PATH: Unbounded_String;
         
         First: Positive := 1;
         Last : Natural  := 1;
      begin
         if not ENV.Exists ("PATH") then
            raise Not_In_Path with
              "PATH environment variable not set.";
         end if;
         
         Set_Unbounded_String
           (Target => PATH,
            Source => ENV.Value ("PATH"));
         
         while First <= Length (PATH) loop
            Last := Index (Source  => PATH,
                           Pattern => ":",
                           From    => First);
            
            if Last < First then
               Last := Length (PATH);
            elsif Last = First then
               raise Not_In_Path with "PATH format error.";
            else
               Last := Last - 1;
            end if;
            
            Path_List.Append (Unbounded_Slice (Source => PATH,
                                               Low    => First,
                                               High   => Last));
            First := Last + 2;
         end loop;
      end;
      
      
      declare
         use Ada.Directories;
         
         Ordinary_Files: constant Filter_Type := (Ordinary_File => True,
                                                  others        => False);
         Search: Search_Type;
         Match : Directory_Entry_Type;
      begin
         -- Search through the paths in order
         for Path of Path_List loop
            
            -- We don't want an exception raised if the indiciated path of
            -- the PATH environment variable is not a valid path, we just
            -- want to skip it
            
            if Exists (To_String (Path)) then
               Start_Search (Search    => Search,
                             Directory => To_String (Path),
                             Pattern   => Program,
                             Filter    => Ordinary_Files);
               
               if More_Entries (Search) then
                  -- We have a match
                  Get_Next_Entry (Search          => Search,
                                  Directory_Entry => Match);
                  
                  return Full_Name (Match);
               end if;
               
               -- Otherwise try the next directory
            end if;
            
         end loop;
      end;
      
      -- If we get here, we didn't find it
      Not_Found_Error (Program);
      
   exception
      when Not_In_Path =>
         raise;
         
      when e: others =>
         raise Not_In_Path with
           "Unable to execute path search due to an exception: " &
           Ada.Exceptions.Exception_Information (e);
         
   end Search_Path;
   
   --
   -- Elaboration_Path_Search
   --
   
   ----------------
   -- Initialize --
   ----------------
   
   function Initialize (Program: aliased String) 
                       return Elaboration_Path_Search 
   is
      use Image_Path_Strings;
   begin
      return Search: Elaboration_Path_Search (Program'Access) do
         
         Search.Result := To_Bounded_String (Search_Path (Program));
         
      exception
         when Ada.Directories.Name_Error =>
            Search.Result := Null_Bounded_String;
            
         when Ada.Strings.Length_Error =>
            raise Ada.Strings.Length_Error with 
              "Full path for program"""
              & Program 
              & """ is too long. Child_Processes.Path_Searching must " 
              & "be modified.";
      end return;
   end Initialize;
   
   -----------
   -- Found --
   -----------
   
   function Found (Search: Elaboration_Path_Search) return Boolean is
      use Image_Path_Strings;
   begin
      return Length (Search.Result) > 0;
   end;
   
   ----------------
   -- Image_Path --
   ----------------
   
   function Image_Path (Search: Elaboration_Path_Search) return String is
      use Image_Path_Strings;
   begin
      if not Found (Search) then
         Not_Found_Error (Search.Program.all);
      else
         return To_String (Search.Result);
      end if;
   end Image_Path;
   
end Child_Processes.Path_Searching;

