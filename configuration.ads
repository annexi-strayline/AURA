------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
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

-- This package contains the types and operations related to the configuration
-- of an AURA subsystem

with Progress;
with Unit_Names;
with Registrar.Subsystems;

package Configuration is
   
   ------------------------
   -- Configuration_Pass --
   ------------------------
   
   procedure Configuration_Pass;
   procedure Process_Root_Configuration;
   Configuration_Progress: aliased Progress.Progress_Tracker;
   
   -- For all Subsystems in the Regsitry of State Aquired, Configuration_Pass
   -- attempts to promote the Subsystem to Available, if possible.
   --
   -- ** Any Subsystems checked-out from "System" repositories are promoted to
   --    "Available" during Checkout, and will not be configured. These
   --    subsystems were configured at build-time for the repository.
   --
   -- The Configuration_Pass process for each Subsystem is as follows:
   --
   -- 1. If a Registry entry for the expected configuration file (AURA.XYZ)
   --    exists, its hash is compared with the hash of the same configuration
   --    from Last_Run (if Last_Run exists). If the hash differs, the process
   --    progresses immediately to Step 3.
   --
   --    If there is no configuration unit but a manifest exists, the process
   --    progresses immediately to Step 2.
   --
   --    If the hash does not differ, the pre-loaded configuration information
   --    is copied from Last_Run, and the process progresses immediately to
   --    Step 4
   --
   --    If there is no configuration unit and no manifest, an empty
   --    configuration unit is generated, and configuration is deemed complete.
   --
   --    -- Root Configuration special case -----------------------------------
   --    The AURA subsystem is handled as a special case, particularily there
   --    may be a configuration file with the unit name "aura.root" The root
   --    configuration unit never has a manifest.
   --    ----------------------------------------------------------------------
   --
   --    If Process_Root_Configuration is invoked, the AURA subsystem is
   --    entered for configuration.
   --
   --    If the root configuration package exists, and the hash differs, then
   --    Step 3 is invoked for it, otherwise Step 4 is invoked.
   --
   --    The Root Configuration package shall not have a Codepaths package.
   --
   -- 2. Step 2 installs the default configuration unit from the manifest.
   --
   --    Step 2 is always followed by Step 3
   --
   -- 3. Step 3 attempts to build, run, and extract the relevent values from
   --    the configuration unit. 
   --
   --    a) The configuration unit is scanned for relevent configuration
   --       parameters. If no relevent parameters are found, configuration is
   --       deemed complete.
   --
   --       Currently, relevent configuration parameters must always and only
   --       be constant String objects. If this is violated, the process is
   --       aborted.
   --
   --    b) The relevant configuration parameters are then used to auto-
   --       generate a temporary program to extract the value of those
   --       parameters. If the program fails to compile, the process is
   --       aborted. If the program completes abnormally, the  process is
   --       aborted.
   --
   --       If the program completes successfully, the configuration values
   --       have been loaded, Configuration is complete, and the Subsystem is
   --       updated in the registry
   --
   -- 4. The subsystem's configuration data has been loaded. If there are any
   --    codepaths configured, those subdirectories are entered with the
   --    registrar. Configuration is deemed complete, and the Subsystem becomes
   --    "Available"
   --
   -- Callers to Configuration_Pass should wait for
   -- Registrar.Registration.Entry_Progress after Configuration_Progress,
   -- except for Process_Root_Configuration
   
   
   -- Utility functions
   function Config_Unit_Name (Target: Registrar.Subsystems.Subsystem)
                             return Unit_Names.Unit_Name;
   -- Returns the unit name of the configuration unit for a given subsystem
   
   function Manifest_Unit_Name (Target: Registrar.Subsystems.Subsystem)
                               return Unit_Names.Unit_Name;
   -- Returns the unit name of the manifest unit for a given subsystem
   
end Configuration;
