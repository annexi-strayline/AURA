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

with Ada.Assertions;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Streams;
with Ada.Directories;
with Ada.Containers.Ordered_Sets;

with Workers, Workers.Reporting;
with Unit_Names;
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
      
      function  Extract_All return Repository_Vectors.Vector;
      
      procedure Add (New_Repo : in     Repository;
                     New_Index:    out Repository_Index);
      
      procedure Update (Index  : in Repository_Index;
                        Updated: in Repository);
      
      procedure Update_Cache_State (Index    : in Repository_Index;
                                    New_State: in Repository_Cache_State);
      
      procedure Request_Cache (Index: in Repository_Index);
      
      procedure Clear;
      
   private
      Repo_Vec: Repository_Vectors.Vector;
   end All_Repositories;
   
   
   protected body All_Repositories is
      
      -----------
      -- Total --
      -----------
      
      function Total return Repository_Count is 
        (Repository_Count (Repo_Vec.Length));
      
      -------------
      -- Extract --
      -------------
      
      function Extract (Index: Repository_Index) return Repository is 
        (Repo_Vec(Index));
      
      -----------------
      -- Extract_All --
      -----------------
      
      function Extract_All return Repository_Vectors.Vector is
        (Repo_Vec);
      
      ---------
      -- Add --
      ---------
      
      procedure Add (New_Repo : in     Repository;
                     New_Index:    out Repository_Index) 
      is begin
         Repo_Vec.Append (New_Repo);
         New_Index := Repository_Index (Repo_Vec.Length);
      end Add;
      
      -------------
      -- Replace --
      -------------
      
      procedure Update (Index  : in Repository_Index;
                        Updated: in Repository)
      is begin
         Repo_Vec (Index) := Updated;
      end Update;
      
      
      ------------------------
      -- Update_Cache_State --
      ------------------------
      
      procedure Update_Cache_State (Index    : in Repository_Index;
                                    New_State: in Repository_Cache_State)
      is begin
         Repo_Vec (Index).Cache_State := New_State;
      end Update_Cache_State;
      
      -------------------
      -- Request_Cache --
      -------------------
      
      procedure Request_Cache (Index: in Repository_Index) is
      begin
         if Repo_Vec (Index).Cache_State = Standby then
            Repo_Vec (Index).Cache_State := Requested;
         end if;
      end Request_Cache;
      
      -----------
      -- Clear --
      -----------
      
      procedure Clear is
      begin
         Repo_Vec.Clear;
      end Clear;
   
   end All_Repositories;
   
   
   --
   -- Generation and Parsing Operations
   --
   
   procedure Validate_AURA_Spec 
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
   is separate;
   
   -- Verifies the correct format of the AURA package, as well as
   -- checking that the Repository_Format type matches the definition
   -- within this build.
   --
   -- An exception, typically Assertion_Error is raised if this validation
   -- fails.
   
   
   procedure Generate_AURA_Spec is separate;
   
   -- Generates a new AURA spec in the 'aura' subdirectory of the project root,
   -- and then submits it to the Registrar.
   
   
   procedure Parse_Repo_Spec 
     (Stream       : not null access Ada.Streams.Root_Stream_Type'Class;
      Expected_Name: in     Unit_Names.Unit_Name;
      Repo         :    out Repository) 
   is separate;
   
   -- Validates and extracts the data from a repository Ada spec.
   -- The parsed Repository is not added to All_Repositories to allow
   -- some additional checks in the case of the "default" Root repository
   
   
   procedure Generate_Repo_Spec (Index: Repository_Index) is separate;
   
   -- See specification (private part)
   
   
   procedure Load_Repository 
     (Repo_Spec     : in Registrar.Library_Units.Library_Unit;
      Expected_Index: in Repository_Index);

   -- Verifies and loads the specified Repository, and then appends it to
   -- All_Repositories.
   --
   -- This is not done in parallel for a few reasons:
   -- 1. Normally, one does not expect a large number of repos.
   -- 2. Repo specs do not take much to parse.
   -- 3. If done in parallel, the repositories must still be entered to
   --    All_Repositories sequentially, anyways. This induces overhead.
   --    Considering points 1 + 2, that overhead would likely nullify any
   --    gains in most cases. If a project has hundreds of repositories,
   --    it might be better for the user to do some house-keeping.
   
   
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
   
   --------------------------
   -- Init_AURA_Spec_Order --
   --------------------------
   
   function Image (Order: Init_AURA_Spec_Order) return String is
     ("[Init_AURA_Spec_Order] (Repositories)");
   
   procedure Execute (Order: in out Init_AURA_Spec_Order) is
      use Registrar.Library_Units;
      use Registrar.Source_Files;
      use Unit_Names;
      
      
      AURA_Unit_Name: Unit_Name := Set_Name ("AURA");
      AURA_Spec_Unit: Library_Unit;
   begin
      -- Check for the expected root "AURA" package
      
      if Reg_Qs.Unit_Entered (AURA_Unit_Name) then
         AURA_Spec_Unit := Reg_Qs.Lookup_Unit (AURA_Unit_Name);
         
         Assert (Check   => AURA_Spec_Unit.Spec_File /= null,
                 Message => "AURA package has no specification.");
         
         Assert (Check   => AURA_Spec_Unit.Body_File = null,
                 Message => "AURA package shall not have a body.");
         
         declare
            AURA_Spec_Stream: aliased Source_Stream
              := Checkout_Read_Stream (AURA_Spec_Unit.Spec_File);
         begin
            Validate_AURA_Spec (AURA_Spec_Stream'Access);
            -- Will raise the appropriately messaged exception if the
            -- validation fails
         end;
         
      else
         Generate_AURA_Spec;
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
      
   begin
      -- We need to collect a list of all repository files (AURA.Repository_X)
      -- registered during the root registration process, and then sort that
      -- list before parsing each one in order. The first spec (Root Repo) is
      -- special, must be either validated, or generated
      
      -- Sort (and filter) the unsorted list by adding each
      -- aura.repository_x unit to the set, which will cause them to
      -- be ordered appropriately
      for Unit of Unsorted_List loop
         if Unit.Name.Match_Initial ("aura.repository_") then
            Assert (Check => Unit.Spec_File /= null,
                    Message => "Repository unit """
                      &        Unit.Name.To_UTF8_String
                      &        """ has no specification");
            Assert (Check => Unit.Body_File = null,
                    Message => "Repository unit """
                      & Unit.Name.To_UTF8_String
                      & """ shall not have a body");
            Sorted_List.Insert (Unit);
         end if;
      end loop;
      
      -- An empty sorted list simply means that there are no repository
      -- specs, and therefore we need to create the Root Repo's spec,
      -- and call it a day.
      if Sorted_List.Length = 0 then
         declare
            New_Index: Repository_Index;
         begin
            All_Repositories.Add (New_Repo  => Root_Repository_Actual,
                                  New_Index => New_Index);
         
            Generate_Repo_Spec (New_Index);
            return;
         end;
      end if;
      
      -- There should never be any gaps in the sorted list. If Repository_1
      -- (Root repository)
      -- missing, we'd expect there to be no repository specs at all, and hence
      -- we dealt with that condition already. If we're here now we shall, at
      -- the very least have Repository_1.
      
      declare
         I: Repository_Index := Root_Repository;
      begin
         for Unit of Sorted_List loop
            Assert (Check   => Unit.Name = Expected_Unit_Name (I),
                    Message => "Missing repository unit for "
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
         Expected_Index: Repository_Index := Root_Repository;
      begin
         for Unit of Sorted_List loop
            Load_Repository (Repo_Spec      => Unit,
                             Expected_Index => Expected_Index);
            Expected_Index := Expected_Index + 1;
         end loop;
      end;
      
   end Execute;
   
   ---------------------
   -- Load_Repository --
   ---------------------
   
   procedure Load_Repository 
     (Repo_Spec     : in Registrar.Library_Units.Library_Unit;
      Expected_Index: in Repository_Index)
   is
      use Registrar.Source_Files;
      
      Repo_Spec_Stream: aliased Source_Stream
        := Checkout_Read_Stream (Repo_Spec.Spec_File);
      New_Repo : Repository;
      New_Index: Repository_Index;
   begin
      Parse_Repo_Spec (Stream        => Repo_Spec_Stream'Access,
                       Expected_Name => Expected_Unit_Name (Expected_Index),
                       Repo          => New_Repo);
      
      if New_Repo.Format = System then
         -- System repositories need to have their cache state to Requested
         -- This ensures that the repo is scanned every time, so that
         -- the user can be alerted. This is necessary since System repos
         -- checkout subsystems via filesystem symlinks.
         New_Repo.Cache_State := Requested;
         
      elsif Expected_Index = Root_Repository then
         -- The Root Repo needs to be "automatically" checked-out, and
         -- compared against the expected (hard-coded) representation
         
         New_Repo.Cache_State := Available;
         New_Repo.Cache_Path := New_Repo.Location;
         
         Assert (Check   => New_Repo = Root_Repository_Actual,
                 Message => "The Root Repository (Repostory" 
                   & Repository_Index'Image (Root_Repository) 
                   & ") does not contain the correct values. " 
                   & "Please delete and rerun aura to regenerate.");
         
      end if;
         
      All_Repositories.Add (New_Repo, New_Index);
      pragma Assert (New_Index = Expected_Index);
      Initialize_Repositories_Tracker.Increment_Completed_Items;
      
   exception
      when others =>
         Initialize_Repositories_Tracker.Increment_Failed_Items;
      
   end Load_Repository;
   
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
   
   -----------------
   -- Extract_All --
   -----------------
   
   function Extract_All return Repository_Vectors.Vector
     is (All_Repositories.Extract_All);
   
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
      Generate_Repo_Spec (New_Index);
   end;
   
   -----------------------
   -- Update_Repository --
   -----------------------
   
   procedure Update_Repository (Index  : in Repository_Index;
                                Updated: in Repository)
   is begin
      All_Repositories.Update (Index, Updated);
      Generate_Repo_Spec (Index);
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
   
end Repositories;
