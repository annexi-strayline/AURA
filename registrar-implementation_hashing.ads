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

-- This package manages the generation of collective Implementation_Hash
-- component of all Library_Units in the Registry that are not Requested.
--
-- The Implementation_Hash tracks which units can be recompiled without
-- recompiling units that depend on those units. If the implementation has been
-- modified, but the specification has not, only the bodies of the
-- implementation of that unit need to be recompiled.

with Progress;
with Registrar.Library_Units;

package Registrar.Implementation_Hashing is
   
   procedure Hash_All;
   procedure Hash_Configurations;
   
   -- Hash_All is intended to be executed prior to the build phase of AURA,
   -- and takes a subset of all Library_Units (including any Subunits)
   -- that do not have a state of Requested.
   --
   -- Hash_Configurations is intended to be used during the checkout cycle
   -- of AURA, and hases on the configuration and manifest units of each
   -- Subsystem with a state of "Aquired"
   --
   -- For both executions, an appropriate library unit subset is generated and
   -- work orders are dispatched to collect the hashes for the bodies and
   -- subunits and enter them into the appropriate collection queue, which are
   -- finally reduced to the collective hash, which is then set per library
   -- unit (not for any subunits), and updated in the registry
   --
   -- To assist in Last_Run hash comparisons, Hash_Subset also copies the
   -- Hash property of the Spec_File to the Specification_Hash property.
   --
   -- Note that for units that do not have bodies or subunits, the resulting
   -- Implementation_Hash will not be valid, but the Specification_Hash will
   -- still be copied-out. Similarily, for units such as subunits that do not
   -- have specifications, Specification_Hash will not be valid. 
   --
   -- Calling Hash_Subset when a pass is already running, but is not yet
   -- complete, causes Program_Error to be propegated

   
   -- Trackers --
   --------------
   Collection_Phase_Progress: aliased Progress.Progress_Tracker;
   -- Collection_Phase_Progress tracks the number of units that have their
   -- hashes collected. The total is equal to the size of the subset
   -- (in Library_Units)
   
   Crunch_Phase_Progress    : aliased Progress.Progress_Tracker;
   -- Crunch_Phase_Progress tracks the library units (excluding subunits) that
   -- will receive a collective hash value for Implementation_Hash, and have
   -- that hash updated in the Registry
   
   -- Both trackers are set before work orders are submitted to the worker pool
   -- and so waiting on Crunch_Phase_Progress may be done without first waiting
   -- for Collection_Phase_Progress to complete
   
   
end Registrar.Implementation_Hashing;
