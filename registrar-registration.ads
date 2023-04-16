------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019-2023, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- The Registration package provides the interfaces for the submission of
-- requests that modify or add to the Registry (All_Subsystems or
-- All_Library_Units) 

with Ada.Directories;

with Progress;
with Unit_Names;
with Registrar.Subsystems;
with Registrar.Library_Units;

package Registrar.Registration is
   
   ----------------
   -- Unit Entry --
   ----------------
   
   -- Unit entry takes the Directory_Entry of a source file (library unit),
   -- or and takes the one of following actions:
   --
   -- A. If the unit's file name ends in .ads or .adb, it is processed as 
   --    follows:
   --    1. All "with" statments are analyzed and the relevent units are
   --       requested (including subsystems)
   --    2. All "pragma External_With" statements are analyzed, and the
   --       relevant non-ada source file is requested
   --    3. The unit's unit name and subsystem is extracted and registered
   --
   -- B. If the unit's file name has any other extention, it is registered as
   --    a non-ada unit, to be potentially included by an External_With pragma
   
   -- Unit entries are processed asynchronously by the Worker tasks.
   
   procedure Enter_Unit (File: in Ada.Directories.Directory_Entry_Type);
   -- Enter a project (non-AURA) source file.
   
   procedure Enter_Unit 
     (File          : in Ada.Directories.Directory_Entry_Type;
      AURA_Subsystem: in Subsystems.Subsystem);
   -- Enters an unit that is expected to be part of an AURA subsystem, 
   -- typically through aquisition.
   --
   -- The AURA Subsystem must be first registered with Enter_AURA_Subsystem
   -- below, and the unit must then be a member of that Subsystem.
   --
   -- For External_With pragmas, the withed external unit is taken to be
   -- part of AURA_Subsystem, and thus is requested as Subsystem.Name%file.ext
   --
   -- All non-ada units are registered as part of AURA_Subsystem, and thus
   -- are registered as Subsystem.Name%file.ext
   
   procedure Enter_Root;
   -- Iterates through the root (current) directory, and dispatches to
   -- Enter_Unit for each recognized source file type
   
   procedure Enter_All_AURA;
   -- Iterates through the "aura" subdirectory of the root directory, and
   -- dispatches to Enter_Unit for each recognized source file type.
   --
   -- This behaves similar to Root, as it is not explicitly registered as
   -- an "aura" subsystem like is requred for Enter_Directory.
   --
   -- The 'aura' subdirectory is expected to exist. This is normally handled
   -- by the Scheduling subsystem.
   
   use type Ada.Directories.File_Kind;
   
   procedure Enter_Directory 
     (Directory     : in Ada.Directories.Directory_Entry_Type;
      AURA_Subsystem: in Subsystems.Subsystem)
   with Pre => Ada.Directories.Kind (Directory) = Ada.Directories.Directory;
   -- Iterates through a directory (not including subdirectories), and invokes
   -- Enter_Unit on each recognized source file type
   
   
   -- Progress Trackers --
   -----------------------
   Entry_Progress: aliased Progress.Progress_Tracker;
   
   -- Tracks the total amount of work orders handled by the entire entry
   -- process. This tracker is not reset by the registrar. Waiting on
   -- completion of this tracker after making an entry submission indicates
   -- that the work associated with that entry is complete
   
   
   ------------------------
   -- Non-entry Services --
   ------------------------
   
   procedure Request_AURA_Subsystem (Name: in     Unit_Names.Unit_Name; 
                                     OK  :    out Boolean);
   -- Specifically requests a particular AURA subsystem (derrived from the
   -- subsystem name of Name)
   --
   -- If that subsystem is already registered a check is made that the existing
   -- subsystem is also an AURA subsystem. If that check fails, OK is set to 
   -- False.
   -- 
   -- If the subsystem is succuessfully requested, or it is already entered and
   -- also an AURA subsystem, then OK is True
   --
   -- This subprogram is primarily used by the CLI driver to allow the user to
   -- explicitly checkout subsystems
   
   procedure Update_Subsystem (Update: in Subsystems.Subsystem);
   -- Updates (replaces) an existing Subsystem in the Registry
   
   procedure Update_Library_Unit (Update: in Library_Units.Library_Unit);
   -- Updates (replaces) an existing Library_Unit in the Registry
   
   procedure Update_Library_Unit_Subset 
     (Update: in Library_Units.Library_Unit_Sets.Set);
   -- Updates (replaces) all existing Library_Units in the Registry that
   -- are also in the Update set. If any units in the Subset are not also in
   -- the Registry, Constraint_Error is raised
   
   procedure Exclude_Manifests;
   -- Removes all Configuration Manifest units from all Subsystems.
   -- This should be invoked after checkout, and before compilation
   

   
end Registrar.Registration;
   
