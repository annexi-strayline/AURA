------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2023, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

-- This package centralizes a series of subprograms that are invoked at
-- the most appropriate locations to handle depreciated features or
-- configurations. 
--
-- Depreciation handler subprograms should contain all necessary checking
-- and processing, and even user queries to deal with a specific depreciated
-- feature.
--
-- Depreciaion handlers are required to be capable of patching any given
-- AURA project to remove depreciated conditions, or to provide guidence
-- to the user how to make their project complient.
--
-- ** Depreciation that cannot be patched causes any affected aura operation
--    to fail.

package Depreciation_Handlers is
   
   procedure AURA_Subdirectory (OK_To_Proceed: out Boolean);
   
   -- In early betas of AURA, the aura subsystem files were typically stored
   -- in the project root. This ends up polluting the user's project with
   -- files that might be better placed in their own subsystem subdirectory.
   -- This is afterall where all other non-root subsystems normally go.
   --
   -- AURA does some special processing of aura subsystem units, particularly
   -- during genration, and so this is a permanent change (depreciaion).
   --
   -- This hander is run as part of the new Scheduling.Enter_Project, which
   -- replaced (the public) Enter_Root. The new Enter_Project will run this
   -- handler, and abort or continue as appropriate.
   --
   -- First all units in the project root are entered, and then this handler
   -- is invoked. if the aura subsystem is thereafter registered (indicating
   -- AURA subsystem files exist in the project root), this handler prompts the
   -- user to agree to to moving all AURA subsystem units to an aura
   -- subdirectory.
   --
   -- Having AURA subsystem files in root is fully depreciated, and therefore
   -- if the user rejects moving the AURA subsystem files via the prompt,
   -- the AURA CLI aborts.
   --
   -- If the user rejects, or if an error occurs, OK_To_Proceed is False.
   
end Depreciation_Handlers;

