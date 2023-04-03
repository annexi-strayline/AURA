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

-- This package handles all actions related to the AURA repositories registered
-- in a given AURA project, including checking-out subsystems, and verifying
-- previously checked-out subsystems

with Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with Progress;

package Repositories is
   
   type    Repository_Count is new Natural;
   subtype Repository_Index is Repository_Count 
     range 1 .. Repository_Count'Last;
   
   Root_Repository: constant Repository_Index := Repository_Index'First;
   -- This repository automatically exists in all AURA projects, and is used to
   -- detach subsystems from any upstream repository, or to handle locally
   -- created subsystems
   
   type Repository_Format is 
     (System,
      -- System repositories are special kinds of "local" repositories that
      -- have been precompiled into a set of per-subsystem shared libraries.
      --
      -- The central repository location (which may not be the current
      -- directory) is expected to contain .ali files for all units within
      -- each subsystem, and a shared library file corresponding to each
      -- subsystem in a directory named "aura_lib".
      --
      -- Central repositories are always cached (checked for changes). Any
      -- changes must be explicitly accepted by the user for any build process
      -- to succeed.
      --
      -- Checked-out subsystems from System repositories are formed as
      -- symbolic links, and are never compiled.
      --
      -- Subsystems checked-out from a System repository shall not have any
      -- dependencies on subsystems from any other repository. This is checked
      -- after all subsystems are checked-out, so it is up to the user to
      -- ensure that requisite subsystems are checkedout out from the same
      -- repository, or the process will fail.
      
      Local,
      -- Local repositories are simply a directory containing some set of
      -- AURA Subsystems (in their respective sub-directories).
      --
      -- Snapshot is a recursive SHA1 hash of all files (except "dot files") in
      -- the repository path, in lexicographical order
      
      Git);
      -- A git repository. Snapshot is the full commit hash.
   
   type Repository_Cache_State is
     (Standby,
      -- The repository has been loaded but has not yet been needed, hence the
      -- cache state has not yet been evaluated
      
      Requested,
      -- The repository is needed to aquire some subsystems
      
      Available);
      -- Repository is cached (git), or exists and has been verified (local)
      
   
   package UBS renames Ada.Strings.Unbounded;
   
   type Repository (Format: Repository_Format := Local) is
      record
         Location       : UBS.Unbounded_String;
         Snapshot       : UBS.Unbounded_String;
         
         Cache_State    : Repository_Cache_State := Standby;
         Cache_Path     : UBS.Unbounded_String;
         -- Full path to the directory containing the repository
         
         case Format is
            when Local | System =>
               null;
               
            when Git =>
               Tracking_Branch: UBS.Unbounded_String;
         end case;
      end record;
   
   package Repository_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Repository_Index,
      Element_Type => Repository);
   
   ---------------------------
   -- Repository Operations --
   ---------------------------
   
   procedure Initialize_Repositories;
   Initialize_Repositories_Tracker: aliased Progress.Progress_Tracker;
   
   -- Shall be called after the Registrar has completed entery of the project
   -- root directory
   --
   -- Initialize_Repositories takes the following actions:
   --
   -- 1. Clears the existing repository vector.
   --
   -- 2. Checks for the existence of the root AURA package (aura.ads).
   --    - If it exists, it is checked for compatibility with this
   --      implementation
   --    - If it does not exist, it is generated, and then entered to the
   --      Registrar.
   --      * Note, the package will be inserted into the 'aura' subdirectory
   --        of the project root. If this directory does not exist, it is
   --        created
   --
   -- 3. Checks for all AURA.Respository_X packages, processes them,
   --    and loads them into the internal list
   --
   -- 4. Verifies the "local" repository (Index 1), and generates the spec
   --    if it does not already exist
   --
   -- Initialize_Repositories is a sequential operation (and generally very
   -- quick), and so does not submit work orders or have a tracker.
   --
   -- After calling Initialize_Repositories, any invalid specifications can
   -- be queried through the Repo's Valid component, and the error message
   -- can be extracted from the Parse_Errors component.
   --
   -- The procedure Check_Repositories and it's associated tracker can be
   -- used to verify 
   --
   -- Initialize_Repositories may enter new units with the registrar.
   
   ------------------------
   -- Repository Queries --
   ------------------------
   
   -- The following operations are all task-safe
   
   function  Total_Repositories return Repository_Count;
   
   function  Extract_Repository (Index: Repository_Index)
                                return Repository;
   
   function  Extract_All return Repository_Vectors.Vector;
   
   function  Cache_State (Index: Repository_Index) 
                         return Repository_Cache_State;
   
   procedure Add_Repository (New_Repo : in     Repository;
                             New_Index:    out Repository_Index);
   -- This also generates the associated spec file
   
   procedure Update_Repository (Index  : in Repository_Index;
                                Updated: in Repository);
   -- The repository at index Index is replaced by Updated. And the
   -- actual spec file is regenerated
   
   procedure Request_Cache (Index: in Repository_Index);
   -- Iff the current Cache is "Standby", it is set to "Requested"
   
private
   
   procedure Update_Cache_State (Index    : in Repository_Index;
                                 New_State: in Repository_Cache_State);
   -- Does not regenerate the spec file
   
   procedure Generate_Repo_Spec (Index: Repository_Index);
   -- Generates the corresponding Spec file for the repository at Index
   -- of the All_Repositories vector. If a file already exists, it is replaced.
   
end Repositories;
