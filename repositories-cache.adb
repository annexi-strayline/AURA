------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
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

with Workers;
with Workers.Reporting;

package body Repositories.Cache is
   
   -- Configuration
   Cache_Root: constant String
     := Ada.Directories.Current_Directory & "/.aura/cache";
   
   -- Useful things
   New_Line: Character renames Workers.Reporting.New_Line;
   
   --
   -- Validate_Local_Or_System
   --
   
   -- Validate_Local_Or_System validates a Local or System repo.
   --
   -- The first step is to verify that Location is accessible, followed by
   -- a collective hash of all non-hidden files contained therein (recursively)
   -- 
   -- For existing repos, the calculated hash is verified. If there is a hash
   -- mismatch, the use is notified interactively, and is given the option
   -- to accept the changes or abort the process.
   --
   -- Changes to System repositories imply potential global changes to all
   -- subsystems already checked-out from that repository
   --
   -- If the user choses to accept changes on a repository, the repository
   -- unit's Snapshot property is updated with the hash
   --
   -- If Snapshot of the dispatched repo is empty, the repo is assumed to be
   -- new.
   --
   -- If Location is not accessible, validation fails
   --
   -- Finally, if the local repo is validated, it's path is used to set
   -- Cache_Path
   
   package Validate_Local_Or_System is
      
      procedure Dispatch (Repo: in Repository; Index: in Repository_Index);
      -- Dispatch submits a work order to validate a local repository
      -- identified by Repo/Index. The process is attached to the
      -- Caching_Progress tracker
      
   end Validate_Local_Or_System;
   
   package body Validate_Local_Or_System is separate;
   
   --
   -- Checkout_Git
   --
   
   -- Checkout_Git ensures that the cache for a given Git repo is both
   -- present and valid.
   --
   -- If the cache is not present, an attempt is made to clone and checkout
   -- the repo. If the cache is present, it is verified to be of the correct
   -- snapshot (commit), and tracking branch. If these are not the expected
   -- value, the existing cache is expunged, and a new cache is checked-out
   
   package Checkout_Git is
      
      procedure Dispatch (Repo: in Repository; Index: in Repository_Index);
      -- Dispatch submits a work order to substantiate a git repository
      -- identified by Repo/Index. The process is attached to the
      -- Caching_Progress tracker
      
   end Checkout_Git;
   
   package body Checkout_Git is separate;

   ------------------------
   -- Cache_Repositories --
   ------------------------
   
   procedure Cache_Repositories is
      Repo_Vec: constant Repository_Vectors.Vector := Extract_All;
   begin
      -- The Root Repository is special, and should never be "cached". In fact,
      -- it always has a cache state of "Available", So let's not waste time
      -- iterating over it
      
      for I in Root_Repository + 1 .. Repository_Index(Repo_Vec.Length) loop
         if Repo_Vec(I).Cache_State = Requested then
            Caching_Progress.Increment_Total_Items;
            
            case Repo_Vec(I).Format is
               when System | Local =>
                  Validate_Local_Or_System.Dispatch 
                    (Repo => Repo_Vec(I), Index => I);
                  
               when Git =>
                  Checkout_Git.Dispatch (Repo => Repo_Vec(I), Index => I);
            end case;
         end if;
      end loop;
      
   end Cache_Repositories;
   
end Repositories.Cache;
