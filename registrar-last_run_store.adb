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
with Ada.Containers.Hashed_Sets;

with Registrar.Registry;

package body Registrar.Last_Run_Store is
   
   All_Subsystems_Store   : constant String := ".aura/all_subsys.dat";
   All_Library_Units_Store: constant String := ".aura/all_libuni.dat";
   
   -- The Operation is simple and fail-passive. Any failure during load simply
   -- leads to an empty set
   
   ----------
   -- Load --
   ----------
   
   generic
      Store_Path: in String;
      with package Sets is new Ada.Containers.Hashed_Sets (<>);
   function Generic_Load_Last_Run return Sets.Set;
   
   function Generic_Load_Last_Run return Sets.Set is
      use Sets;
      use Ada.Streams.Stream_IO;
      
      Store: File_Type;
   begin
      Open (File => Store,
            Mode => In_File,
            Name => Store_Path);
      
      return Loaded_Set: Set do
         Set'Read (Stream (Store), Loaded_Set);
         Close (Store);
      end return;
      
      -- Note that in the case of Source_File_Access types, the Ada stream
      -- attributes have been specified to ensure these do the right thing
      
   exception
      when others =>
         return Empty_Set;
      
   end Generic_Load_Last_Run;
   
   -- All_Subsystems --
   function Load_Last_Run return Subsystems.Subsystem_Sets.Set is
      function Do_Load is new Generic_Load_Last_Run
        (Store_Path => All_Subsystems_Store,
         Sets       => Subsystems.Subsystem_Sets);
   begin
      return Do_Load;
   end Load_Last_Run;
   
   
   -- All_Library_Units --
   function Load_Last_Run return Library_Units.Library_Unit_Sets.Set is
      function Do_Load is new Generic_Load_Last_Run
        (Store_Path => All_Library_Units_Store,
         Sets       => Library_Units.Library_Unit_Sets);
   begin
      return Do_Load;
   end Load_Last_Run;
   
   -----------
   -- Store --
   -----------
   
   generic
      Store_Path: in String;
      with package Sets is new Ada.Containers.Hashed_Sets (<>);
   procedure Generic_Store_Last_Run (S: Sets.Set);
   
   procedure Generic_Store_Last_Run (S: Sets.Set) is
      use Sets;
      use Ada.Streams.Stream_IO;
      
      Store: File_Type;
   begin
      if not Ada.Directories.Exists (".aura") then
         Ada.Directories.Create_Directory (".aura");
      end if;
      
      if not Ada.Directories.Exists (Store_Path) then
         Create (File => Store,
                 Mode => Out_File,
                 Name => Store_Path);
      else
         Open (File => Store,
               Mode => Out_File,
               Name => Store_Path);
      end if;
      
      Set'Write (Stream (Store), S);
      
      Close (Store);
         
   end Generic_Store_Last_Run;
   
   --------------------------------------------------
   procedure Store_Current_Run is
      use Ada.Streams.Stream_IO;
      use type Ada.Containers.Count_Type;
      
      Store: File_Type;
      
      function Select_All (Element: Subsystems.Subsystem)
                          return Boolean is (True);
      
      function Select_All (Element: Library_Units.Library_Unit)
                          return Boolean is (True);

      Current_All_Subsystems: constant Subsystems.Subsystem_Sets.Set
        := Registry.All_Subsystems.Extract_Subset (Select_All'Access);
      
      Current_All_Library_Units: Library_Units.Library_Unit_Sets.Set
        := Registry.All_Library_Units.Extract_Subset (Select_All'Access);
      
      procedure Store_All_Subsystems is new Generic_Store_Last_Run
        (Store_Path => All_Subsystems_Store,
         Sets       => Subsystems.Subsystem_Sets);
      
      procedure Store_All_Library_Units is new Generic_Store_Last_Run
        (Store_Path => All_Library_Units_Store,
         Sets       => Library_Units.Library_Unit_Sets);
      
      
   begin
      Store_All_Subsystems    (Current_All_Subsystems   );
      Store_All_Library_Units (Current_All_Library_Units);
      
   end Store_Current_Run;
   
end Registrar.Last_Run_Store;
