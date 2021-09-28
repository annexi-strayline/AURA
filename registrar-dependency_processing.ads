------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
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

-- This package contains specialized processing steps for the consolidation of
-- the Registrar's unit dependency maps

with Progress;

package Registrar.Dependency_Processing is
   
   ------------------------------
   -- Consolidate_Dependencies --
   ------------------------------
   
   procedure Consolidate_Dependencies;
   Merge_Subunits_Progress: aliased Progress.Progress_Tracker;
   Fan_Out_Progress       : aliased Progress.Progress_Tracker;
   Build_Reverse_Progress : aliased Progress.Progress_Tracker;
   
   -- Consolidate_Dependencies ensures that the Unit/Subsystem Forward/Reverse
   -- dependency maps are consolidated and coherent
   --
   -- Consolidate_Dependencies must be completed before any of the dependency
   -- maps are consulted.
   --
   -- Consolidate_Dependencies executes three phases (basically a map-reduce):
   --
   -- 1. Merge Subunits: All forward dependencies of all explicit Subunit units
   --                    are unioned with the forward dependency map of their
   --                    Library Unit parent.
   --
   --                    The subsystem forward dependency map is
   --                    opportunistically populated during this phase.
   --
   -- 2. Fan-out       : All non-Requested units in the All_Library_Units
   --                    registry have themselves queued for inclusion on the
   --                    reverse dependency map of each forward dependency
   --
   --                    Similarily for all non-Requested Subsystems
   --
   -- 3. Build Reverse : All non-Requested units in the All_Library_Units
   --                    registry have their reverse dependency map sets built
   --                    from the queued names generated in the Fan-out phase
   --
   --                    Similarily for all non-Requested Subsystems
   
end Registrar.Dependency_Processing;
