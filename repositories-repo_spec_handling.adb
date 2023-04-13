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

separate (Repositories)

package body Repo_Spec_Handling is
   
   ---------------------
   -- Parse_Repo_Spec --
   ---------------------
   
   procedure  Parse_Repo_Spec 
     (Stream       : not null access Ada.Streams.Root_Stream_Type'Class;
      Expected_Name: in     Unit_Names.Unit_Name;
      Repo         :    out Repository)
   is separate;
   
   
   ------------------------
   -- Generate_Repo_Spec --
   ------------------------
   
   procedure Generate_Repo_Spec (Index: Repository_Index) is separate;
   
   
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
   
end Repo_Spec_Handling;
