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

-- This package handles the checkout specification files, as well as the
-- checkout action (copying from the cache)

with Progress;
with Registrar.Subsystems;

package Checkout is
   
   -------------------
   -- Checkout_Pass --
   -------------------
   
   procedure Checkout_Pass;
   Checkout_Pass_Progress: aliased Progress.Progress_Tracker;
   
   -- Should be called only after:
   -- * The project has been entered to the Registrar,
   -- * Repositories have been initialized.
   --
   -- This operation interates through all "Requested" AURA Subsystems and
   -- dispatches to a work order per subsystem that completes one of the
   -- following actions, entering all units of the subsystem's root if aquired:
   --
   -- * Checkout spec is available, relevent source directory exists:
   --
   --    The subsystem's Source_Repository is set to that specified by the
   --    Checkout spec. If that index is not valid, the the process fails.
   --    The source contents (exluding subdirectories) are then entered
   --    into the Registrar, and the Subsystem's state is promoted to "Aquired"
   --
   -- * Checkout spec is available, relevent source directory does not exist:
   --
   --    This case is where a checkout operation is actually triggered
   --
   --    The subsystem's Source_Repository is set to that specified by the
   --    Checkout spec. If that index is not valid, the the process fails.
   --
   --    == Source_Repository is not the "root repository":
   --
   --       If Cache_State of Available, the relevent subdirectory is copied
   --       out of the cache (checked-out), and the copied directory is entered
   --       with the Registrar (not including sub-directories).
   --
   --       If the cache does not contain the expected directory, checkout of
   --       the subsystem fails.
   --
   --       If the relevant Repository is not Cached, it is marked as
   --       Requested, and will be checked-out in the next cycle.
   --
   --    == Source_Repository is the "root repository":
   --
   --       All cached repositories starting from index 2 are scanned for the
   --       subsystem, until it is found, the index is not cached, or the index
   --       reaches the end of all repositories.
   --
   --       If the subsystem is found, the repository is set to the index of
   --       the hit, and the checkout spec is written. If the subsystem is not
   --       found, but there remain uncached repositories, then all remaining
   --       uncached repositories are marked as requested. 
   --
   --       If all repositories are cached, and the subsystem cannot be found,
   --       the checkout of the subsystem fails
   --
   --       If the checkout was successful, the subsystem's state is promoted
   --       to "Aquired"
   --
   -- * Checkout spec is not available, relevant source directory exists:
   --
   --    The subsystem's Source_Repository is set to the default "project
   --    local" index of 1, a Checkout spec is generated. The source contents
   --    (exluding subdirectories) are then entered into the Registrar, and the
   --    Subsystem's state is promoted to "Aquired"
   --
   -- * Checkout spec is not available, relevant source directory does not
   --   exist:
   --
   --    The subsystem's Source_Repository is set to the default "project
   --    local" index of 1, and a Checkout spec is generated. The Subsystem's
   --    state is not modified (remains "Requested"). This means that the
   --    next checkout pass will search for an approprite repository
   --
   -- The Checkout_Pass_Progress tracker progress of each candidate checkout
   -- (Requested Subsystem).
   
    
   -------------------------
   -- Write_Checkout_Spec --
   -------------------------
   
   procedure Write_Checkout_Spec (SS: in Registrar.Subsystems.Subsystem);
   -- Writes (or regenerates) the checkout spec file for SS. This is a
   -- sequential operation.
   --
   -- This procedure should be invoked if the Source_Repository value of
   -- a subsystem is changed.
   --
   -- If the spec already exists (the unit is registered), then it is
   -- re-written. If the spec has not been registered, it is entered
   -- to the registrar after generation.
   
end Checkout;
