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

package Repositories.Cache is
   
   procedure Cache_Repositories;
   Caching_Progress: aliased Progress.Progress_Tracker;
   
   -- Attempts to cache all repositories that are marked with a Cache_Status
   -- of Requested.
   --
   -- For Git repositories, any invalid state (wrong commit or branch) causes
   -- any existing cache to be expunged and restored from
   -- the original (Location).
   --
   -- For Local repositories, this is not possible, and instead the user is
   -- queried if they wish to accept the new repository, updating the checkout
   -- hash
   --
   -- The unit of Caching_Progress is a repository
   
   procedure Update_Cache (Index: Repository_Index) is null;
   Update_Progress: aliased Progress.Progress_Tracker;
   
   -- Submit a work order to the worker pool to update the indicated
   -- repository. This applies only to git repositories with a Tracking_Branch.
   --
   -- First, the Tracking branch is checked-out, and then a fetch is executed
   -- on origin (the Repo's Location),followed by a pull operation is executed
   --
   -- If a failure occurs, the repository package is not modified, and
   -- the existing cache is expunged. The status is reverted to Uncached.
   --
   -- The unit of Update_Progress is one Update_Progress request
   
   -- ** Not implemented **
   
   procedure Destroy_Cache (Index: Repository_Index) is null;
   
   -- Deletes the directory containing the identified repository and sets
   -- Cache_Status to Uncached
   
   -- ** Not implemented **
   
end Repositories.Cache;
