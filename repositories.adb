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

with Ada.Streams;
with Ada.Assertions;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Containers.Ordered_Sets;

with Unit_Names;
with User_Notices;
with Workers.Reporting;
with Registrar.Source_Files;
with Registrar.Library_Units;
with Registrar.Subsystems;
with Registrar.Queries;


package body Repositories is
   
   use type Ada.Containers.Count_Type;
   use type Unit_Names.Unit_Name;
   package Reg_Qs renames Registrar.Queries;
   
   procedure Assert (Check  : in Boolean; Message: in String)
     renames Ada.Assertions.Assert;
   
   
   function Expected_Unit_Name (Index: Repository_Index) 
                               return Unit_Names.Unit_Name 
   is (Unit_Names.Set_Name
         ("aura.repository_" 
            & Ada.Strings.Wide_Wide_Fixed.Trim 
              (Source => Repository_Index'Wide_Wide_Image (Index),
               Side   => Ada.Strings.Both)));
   -- Returns the unit name expected for a given repository index
   
   
   New_Line: Character renames Workers.Reporting.New_Line;
   
   Assertion_Error: exception renames Ada.Assertions.Assertion_Error;
   
   
   --
   -- Root_Repository (Repository_1) Definition
   --
   
   Repository_1: constant Repository
     := (Format          => Local,
         Location        => UBS.To_Unbounded_String ("./"),
         Snapshot        => UBS.Null_Unbounded_String,
         
         Cache_State     => Available,
         Cache_Path      => UBs.To_Unbounded_String ("./"));
   
   Root_Repository_Actual: Repository renames Repository_1;
   
   --
   -- All_Repositories
   --
   
   protected All_Repositories is
      function  Total return Repository_Count;
      
      function  Extract (Index: Repository_Index)
                        return Repository;
      
      function  Extract_All return Repository_Maps.Map;
      
      procedure Add (New_Repo : in     Repository;
                     New_Index:    out Repository_Index);
      
      procedure Insert (New_Repo : in Repository;
                        New_Index: in Repository_Index);
      
      procedure Update (Index  : in Repository_Index;
                        Updated: in Repository);
      
      procedure Update_Cache_State (Index    : in Repository_Index;
                                    New_State: in Repository_Cache_State);
      
      procedure Request_Cache (Index: in Repository_Index);
      
      procedure Clear;
      
   private
      Repo_Map: Repository_Maps.Map;
   end All_Repositories;
   
   
   protected body All_Repositories is
      
      -----------
      -- Total --
      -----------
      
      function Total return Repository_Count is 
        (Repository_Count (Repo_Map.Length));
      
      -------------
      -- Extract --
      -------------
      
      function Extract (Index: Repository_Index) return Repository is
        (Repo_Map(Index));
      
      -----------------
      -- Extract_All --
      -----------------
      
      function Extract_All return Repository_Maps.Map is (Repo_Map);
      
      ------------
      -- Insert --
      ------------
      
      procedure Insert (New_Repo : in Repository;
                        New_Index: in Repository_Index)
      is begin
         Repo_Map.Insert (Key      => New_Index,
                          New_Item => New_Repo);
      end Insert;
      
      ---------
      -- Add --
      ---------
      
      procedure Add (New_Repo : in     Repository;
                     New_Index:    out Repository_Index) 
      is
      begin
         if Repo_Map.Length > 0 then
            New_Index := Repo_Map.Last_Key + 1;
         else
            New_Index := Repository_Index'First;
         end if;
         Repo_Map.Insert (Key      => New_Index,
                          New_Item => New_Repo);
      end Add;
      
      -------------
      -- Replace --
      -------------
      
      procedure Update (Index  : in Repository_Index;
                        Updated: in Repository)
      is begin
         Repo_Map (Index) := Updated;
      end Update;
      
      
      ------------------------
      -- Update_Cache_State --
      ------------------------
      
      procedure Update_Cache_State (Index    : in Repository_Index;
                                    New_State: in Repository_Cache_State)
      is begin
         Repo_Map (Index).Cache_State := New_State;
      end Update_Cache_State;
      
      -------------------
      -- Request_Cache --
      -------------------
      
      procedure Request_Cache (Index: in Repository_Index) is
      begin
         if Repo_Map (Index).Cache_State = Standby then
            Repo_Map (Index).Cache_State := Requested;
         end if;
      end Request_Cache;
      
      -----------
      -- Clear --
      -----------
      
      procedure Clear is
      begin
         Repo_Map.Clear;
      end Clear;
   
   end All_Repositories;
   
   
   --
   -- Generation and Parsing Operations
   --
   
   package AURA_Spec_Handling is
      
      procedure Check_AURA_Spec 
        (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
         Correct : out Boolean);
      
      -- Verifies the correct format of the AURA package, as well as
      -- checking that the Repository_Format type matches the definition
      -- within this build. If the existing spec is valid, Correct is set to
      -- True. Otherwise, the user is notified of the issue through the
      -- User_Notices facility, and then Correct is set to False
      
      procedure Generate_AURA_Spec
        (Stream: not null access Ada.Streams.Root_Stream_Type'Class);
      
      -- Generates a new AURA through the given Stream.
      
      procedure Check_Or_Regenerate_AURA_Spec
        (AURA_Spec_Unit: in Registrar.Library_Units.Library_Unit);
      
      -- Executes Check_AURA_Spec on AURA_Main's Spec file, and if found to
      -- be invalid, executes a User Query, asking permission to regenerate
      -- the package.
      --
      -- This operation expects that AURA_Main is appropriate (has a Spec
      -- but no body)
      
      procedure Register_AURA_Spec;
      
      -- Finds and submits the aura specification file, used after invoking
      -- Generate_AURA_Spec on a newly generated spec
      
   end AURA_Spec_Handling;
   
   
   package Repo_Spec_Handling is
      procedure Parse_Repo_Spec 
        (Stream       : not null access Ada.Streams.Root_Stream_Type'Class;
         Expected_Name: in     Unit_Names.Unit_Name;
         Repo         :    out Repository);
      
      -- Validates and extracts the data from a repository Ada spec.
      -- The parsed Repository is not added to All_Repositories to allow
      -- some additional checks in the case of the "default" Root repository
      
      
      procedure Generate_Repo_Spec (Index: Repository_Index);
      
      -- Generates the repo specification for the repository at the given Index,
      -- and enters the generated unit into the Registry
      
      procedure Load_Repository 
        (Repo_Spec     : in Registrar.Library_Units.Library_Unit;
         Expected_Index: in Repository_Index);

      -- Verifies and loads the specified Repository, and then inserts it into
      -- All_Repositories.
      --
      -- This operation is expected to be run (concurrently) from a worker task.
      -- See Load_Repository_Order
      
   end Repo_Spec_Handling;
   
   package body AURA_Spec_Handling is separate;
   package body Repo_Spec_Handling is separate;
   
   --
   -- Work Orders
   --
   
   type Init_AURA_Spec_Order is new Workers.Work_Order with null record;
   
   overriding procedure Execute (Order: in out Init_AURA_Spec_Order);
   overriding function  Image   (Order: Init_AURA_Spec_Order) return String;
   
   -- Ensures the root "aura" package is present and correct, generating if it
   -- necessary
   
   
   type Sort_Repositories_Order is new Workers.Work_Order with null record;
   
   overriding procedure Execute (Order: in out Sort_Repositories_Order);
   overriding function  Image   (Order: Sort_Repositories_Order) return String;
   
   -- Sorts the repository units previously entered to the Registrar, and
   -- ensures there are no gaps. This job also verifies or generates the
   -- Root Repository. All other repositories are dispatched as
   -- Load_Repository_Order's.
   
   
   type Load_Repository_Order is new Workers.Work_Order with
      record
         Index: Repository_Index;
         Unit : Registrar.Library_Units.Library_Unit;
      end record;
   
   overriding procedure Execute (Order: in out Load_Repository_Order);
   overriding function  Image   (Order: Load_Repository_Order) return String;
                                
   -- Loads (parses) a given repository spec and enters it into the
   -- All_Repositories database unit
   
   
   --------------------------
   -- Init_AURA_Spec_Order --
   --------------------------
   
   function Image (Order: Init_AURA_Spec_Order) return String is
     ("[Init_AURA_Spec_Order] (Repositories)");
   
   procedure Execute (Order: in out Init_AURA_Spec_Order) is
      use Registrar.Library_Units;
      use Unit_Names;
      
      use type Registrar.Source_Files.Source_File_Access;
      
      AURA_Unit_Name   : Unit_Name := Set_Name ("AURA");
      AURA_Spec_Unit   : Library_Unit;
      
   begin
      -- Check for the expected root "AURA" package
      
      if Reg_Qs.Unit_Entered (AURA_Unit_Name) then
         AURA_Spec_Unit := Reg_Qs.Lookup_Unit (AURA_Unit_Name);
         
         Assert (Check   => AURA_Spec_Unit.Spec_File /= null,
                 Message => "AURA package has no specification.");
         
         Assert (Check   => AURA_Spec_Unit.Body_File = null,
                 Message => "AURA package shall not have a body.");
         
         AURA_Spec_Handling.Check_Or_Regenerate_AURA_Spec (AURA_Spec_Unit);

      else
         declare
            use Ada.Directories;
            use Ada.Streams.Stream_IO;
            
            AURA_Spec_File: File_Type;
            
            AURA_Subsystem_Directory: constant String
              := Compose (Containing_Directory => Current_Directory,
                          Name                 => "aura");
            
            AURA_Spec_Path: constant String
              := Compose (Containing_Directory => AURA_Subsystem_Directory,
                          Name                 => "aura.ads");
         begin
            -- We do not expect ./aura/aura.ads to exist, since it is
            -- not in the registrar. Earlier steps in the process always
            -- ensure that the 'aura' subdirectory exists in the project
            -- root
            
            Create (File => AURA_Spec_File,
                    Mode => Out_File,
                    Name => AURA_Spec_Path);
            AURA_Spec_Handling.Generate_AURA_Spec (Stream (AURA_Spec_File));
            Close (AURA_Spec_File);
            
         exception
            when others =>
               if Is_Open (AURA_Spec_File) then
                  Close (AURA_Spec_File);
               end if;
               raise;
         end;
            
         AURA_Spec_Handling.Register_AURA_Spec;
      end if;
      
      -- If that worked-out, submit an order for the next step - to
      -- sort and load all the repository packages
      Initialize_Repositories_Tracker.Increase_Total_Items_By (1);
      Workers.Enqueue_Order 
        (Sort_Repositories_Order
           '(Tracker => Initialize_Repositories_Tracker'Access,
             others  => <>));

   end Execute;
   
   -----------------------------
   -- Sort_Repositories_Order --
   -----------------------------
   
   function Image (Order: Sort_Repositories_Order) return String is
     ("[Sort_Repositories_Order] (Repositories)");
   
   procedure Execute (Order: in out Sort_Repositories_Order) is
      use Registrar.Subsystems;
      use Registrar.Library_Units;
      use type Registrar.Source_Files.Source_File_Access;
      
      function Less_Than (Left, Right: Library_Unit) return Boolean
        is (Left.Name < Right.Name);
      
      package Unit_Sorting is new Ada.Containers.Ordered_Sets
        (Element_Type => Library_Unit,
         "<"          => Less_Than);
      
      AURA_SS: constant Subsystem 
        := (AURA   => False,
            Name   => Unit_Names.Set_Name ("AURA"),
            State  => Available,
            others => <>);
      
      Sorted_List  : Unit_Sorting.Set;
      Unsorted_List: Library_Unit_Sets.Set
        := Reg_Qs.Subsystem_Library_Units (AURA_SS);
      
      Repository_1_Regen: Boolean := False;
      
   begin
      -- We need to collect a list of all repository files (AURA.Repository_X)
      -- registered during the root registration process, and then sort that
      -- list before parsing each one in order. The first spec (Root Repo) is
      -- special, must be either validated, or generated
      
      -- Sort (and filter) the unsorted list by adding each
      -- aura.repository_x unit to the set, which will cause them to
      -- be ordered appropriately
      
      -- If we find a repository unit that is "Requested", it typically
      -- indicates that a checkout exists for a repository, but the actual
      -- repospec is missing or has somehow gone wrong.
      
      for Unit of Unsorted_List loop
         if Unit.Name.Match_Initial ("aura.repository_") then
            Assert (Check => Unit.State /= Requested,
                    Message => "Repository spec "
                      & ''' & Unit.Name.To_UTF8_String & "' "
                      & "was expected but not found. "
                      & "This typically indicates that one or more checkout "
                      & "specs (in the 'aura' subdirectory) reference the "
                      & "missing repository.");
            Assert (Check => Unit.Spec_File /= null,
                    Message => "Repository unit '"
                      &        Unit.Name.To_UTF8_String
                      &        "' has no specification");
            Assert (Check => Unit.Body_File = null,
                    Message => "Repository unit '"
                      & Unit.Name.To_UTF8_String
                      & "' shall not have a body");
            Sorted_List.Insert (Unit);
         end if;
      end loop;
      
      
      -- Special Repository_1 Handling.
      --
      -- An empty sorted list simply means that there are no repository
      -- specs, and therefore we need to create the Root Repo's spec,
      -- and call it a day. If it is > 0, we ensure that Repo_1 exists,
      -- and if not it is generated. 
      
      if Sorted_List.Length = 0
        or else Sorted_List(Sorted_List.First).Name
                  /= Expected_Unit_Name (Root_Repository)
      then
         -- Either there are no repositories, or the first repo is not 1.
         -- Both cases need the default created
         
         -- Now we can register it internally directly, and the just generated
         -- version will be parsed normally on the next run of AURA CLI. This
         -- avoids all sorts of complexity if we tried to actually enter the
         -- just-generated verion (specifically the mechanics of registration)
         --
         -- Remember that Repo_Spec_HandlingLoad_Repo_Spec already ensures that
         -- Repository_1 does actually match Root_Repository_Actual.
         

         All_Repositories.Insert (New_Repo  => Root_Repository_Actual,
                                  New_Index => Root_Repository);
         Repo_Spec_Handling.Generate_Repo_Spec (Root_Repository);
      else
         -- Root Repo spec exists - load it now (to verify correctness), and
         -- then remove it from the list.
         declare
            use Unit_Sorting;
            Root_Spec: Cursor := Sorted_List.First;
         begin
            Initialize_Repositories_Tracker.Increase_Total_Items_By (1);
            Workers.Enqueue_Order
              (Load_Repository_Order'
                 (Tracker => Initialize_Repositories_Tracker'Access,
                  Index   => Root_Repository,
                  Unit    => Sorted_List(Root_Spec)));
            Sorted_List.Delete (Root_Spec);
         end;
      end if;
      
      -- There should never be any gaps in the sorted list. Root Repository
      -- is excluded.
      
      declare
         I: Repository_Index := Root_Repository + 1;
      begin
         for Unit of Sorted_List loop
            Assert (Check   => Unit.Name = Expected_Unit_Name (I),
                    Message => "Missing expected repository spec for "
                      &        "Repository" & Repository_Index'Image (I)
                      &        " ("
                      &        Expected_Unit_Name (I).To_UTF8_String
                      &        ")");
            I := I + 1;
         end loop;
      end;
      
      -- Good to go
      Initialize_Repositories_Tracker.Increase_Total_Items_By 
        (Natural (Sorted_List.Length));
      
      declare
         Expected_Index: Repository_Index := Root_Repository + 1;
      begin
         for Unit of Sorted_List loop
            Workers.Enqueue_Order
              (Load_Repository_Order'
                 (Tracker => Initialize_Repositories_Tracker'Access,
                  Index   => Expected_Index,
                  Unit    => Unit));
            Expected_Index := Expected_Index + 1;
         end loop;
      end;
   end Execute;
   
   
   ---------------------------
   -- Load_Repository_Order --
   ---------------------------
   
   function Image (Order: Load_Repository_Order) return String is
     ("[Load_Repository_Order] Expected Index =" 
        & Repository_Index'Image (Order.Index)
        & " (" & Order.Unit.Name.To_UTF8_String & ')');
   
   procedure Execute (Order: in out Load_Repository_Order) is
   begin
      Repo_Spec_Handling.Load_Repository (Repo_Spec      => Order.Unit,
                                          Expected_Index => Order.Index);
   end Execute;
   
   
   --
   -- Package Implementations
   --
   
   -----------------------------
   -- Initialize_Repositories --
   -----------------------------
   
   procedure Initialize_Repositories is
   begin
      All_Repositories.Clear;  -- Clean slate
      Initialize_Repositories_Tracker.Increase_Total_Items_By (1);
      
      Workers.Enqueue_Order 
        (Init_AURA_Spec_Order
           '(Tracker => Initialize_Repositories_Tracker'Access,
             others  => <>));
      -- This work order will launch the next on completion.
      
   end Initialize_Repositories;
   
   
   ------------------------
   -- Total_Repositories --
   ------------------------
   
   function Total_Repositories return Repository_Count
     is (All_Repositories.Total);
   
   
   ------------------------
   -- Extract_Repository --
   ------------------------
   
   function Extract_Repository (Index: Repository_Index)
                               return Repository
     is (All_Repositories.Extract (Index));
   
   
   ------------------------------
   -- Extract_All_Repositories --
   ------------------------------
   
   function Extract_All_Repositories return Repository_Maps.Map
   is
      use type Ada.Containers.Count_Type;
   begin
      return All_Repos: Repository_Maps.Map := All_Repositories.Extract_All do
         pragma Assert
           (if All_Repos.Length > 0 then
               Repository_Index(All_Repos.Length) = All_Repos.Last_Key);
         -- Correct structure of AURA should ensure  that Extract_All is only
         -- ever invoked after Initialize_Repositories completes fully.
         --
         -- The Initialize_Repositories process is responsible for ensuring 
         -- that all existing repository specs are valid, and there are no
         -- holes in the progression of indexes.
         --
         -- Ergo if this assertion fails, AURA CLI has a bug.
      end return;
   end Extract_All_Repositories;
   
   
   -----------------
   -- Cache_State --
   -----------------
   
   function  Cache_State (Index: Repository_Index) 
                         return Repository_Cache_State
   is
      Repo: constant Repository := All_Repositories.Extract (Index);
   begin
      return Repo.Cache_State;
   end Cache_State;
   
   
   --------------------
   -- Add_Repository --
   --------------------
   
   procedure Add_Repository (New_Repo : in     Repository;
                             New_Index:    out Repository_Index)
   is begin
      All_Repositories.Add (New_Repo, New_Index);
      Repo_Spec_Handling.Generate_Repo_Spec (New_Index);
   end;
   
   
   -----------------------
   -- Update_Repository --
   -----------------------
   
   procedure Update_Repository (Index  : in Repository_Index;
                                Updated: in Repository)
   is begin
      All_Repositories.Update (Index, Updated);
      Repo_Spec_Handling.Generate_Repo_Spec (Index);
   end Update_Repository;
   
   
   -------------------
   -- Request_Cache --
   -------------------
   
   procedure Request_Cache (Index: in Repository_Index) is
   begin
      All_Repositories.Request_Cache (Index);
   end Request_Cache;
   
   
   ------------------------
   -- Update_Cache_State --
   ------------------------
   
   procedure Update_Cache_State (Index    : in Repository_Index;
                                 New_State: in Repository_Cache_State)
   is begin
      All_Repositories.Update_Cache_State (Index, New_State);
   end Update_Cache_State;
   
   
   ------------------------
   -- Generate_Repo_Spec --
   ------------------------
   
   procedure Generate_Repo_Spec (Index: Repository_Index)
     renames Repo_Spec_Handling.Generate_Repo_Spec;
   
end Repositories;
