------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
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

-- This package is driven by the aura cli main program (aura.adb) specifically,
-- and is designed to be executed sequentially, in an apprpriate order
-- (determined by the main program). As such, it maintains a state via the
-- public "Command" object (below), as well as an internal Parameters object
-- (of type Parameters_Set)

with Build;
with Registrar.Library_Units;

package Scheduling is
   
   Process_Failed: exception;
   -- Raised during "normal", but ugly failures that are reported via work
   -- reports, or involve explicit checks. 
   --
   -- Generally speaking, if this exception occurs, it is not safe to save the
   -- Registry or Configuration.
   
   Build_Failed: exception;
   -- Raised specifically when any unit fails to compile, bind, or link. Unlike
   -- Process_Failed, this error implies that the Registry and Configuration
   -- can (and should) be safely saved so that subsequent invokcations do not
   -- need to recompile everything.
   
   ----------------
   -- Parameters --
   ----------------
   
   type Selected_Command is 
     (Build_Command,
      Checkout_Command,
      Clean_Command,
      Compile_Command,
      Help_Command,
      Library_Command,
      Run_Command,
      Systemize_Command);
   
   Command: Selected_Command := Checkout_Command;
   -- Set by Initialize_Parameters;
   
   procedure Initialize_Parameters;
   -- Loads the previous set of parameters (if any), and then applies and
   -- validates the parameters passed in.   
   --
   -- Additionally checks the correctness of the specified
   -- options/configuration for relevent commands.
   --
   -- If the parameters are not valid, a message explaining why is output,
   -- and Process_Failed is raised, otherwise Command is set to the selected
   -- command
   --
   -- Initialize_Parameters sets a variety of global variables in the
   -- Scheduling and UI_Primitives packages, and must be invoked before
   -- executing any other subprograms in this package
   
   ---------------
   -- Processes --
   ---------------
   
   procedure Clean;
   -- Removes all state from the project.
   -- 1. Removes the .aura subdirectory, containing all saved state (config and
   --    last-run
   -- 2. Removes the aura-build subdirectory

   procedure Enter_Project;
   -- Enters all units in the root directory, as well as the aura subdirectory
   
   procedure Initialize_Repositories;
   
   procedure Add_Explicit_Checkouts;
   -- Enters "Requested" subsystems into the Registrar if they are not already
   -- registered
   --
   -- This should be called after Enter_Root to ensure that root subsystems are
   -- not checked-out by this process.
   --
   -- If there are no explicit checkouts to add, no action is taken.
   
   procedure Checkout_Cycle;
   -- Goes through successful cycles of Checkout -> Hash -> Configure -> Cache
   -- until no Subsystems are in a state of "Requested".
   --
   -- If three cycles pass where the number of requested subsystems does not
   -- change, a list of all requested subsystems is output and Process_Failed
   -- is raised.
   --
   -- After a successful Checkout_Cycle process (all Requested subsystems
   -- aquired and configured), the Root Configuration is processed (if any) and
   -- then all Configuration Manifests are excluded from the Registrar.
   
   procedure Consolidate_Dependencies;
   -- Consolidates and builds all dependency maps
   
   procedure Check_Completion;
   -- Check for any subsystems that are Unavailable (checkout failed for
   -- "common" reasons), and report diagnostic information to the user
   -- before aborting.
   --
   -- If all Subsystems are Available, Check for any units with a state of
   -- Requested (there should be none), and report any found to the user before
   -- aborting
   --
   -- If -v is not given as a parameter, specific dependency relationships are
   -- not output, and a simiplified description of incomplete or unavailable
   -- subsystems is output.
   --
   -- If there are checkout failures and Verbose will be False,
   -- Consolidate_Dependencies does not need to be invoked first, otherwise
   -- it must be invoked first.
   
   procedure Hash_Registry;
   -- Hash all units in the registry, including compilation products
   
   procedure Compile;
   -- Executes:
   -- 1. Build.Compute_Recomplations
   -- 2. Build.Compilation.Compile
   --
   -- If Quiet is True, compiler output is not copied to the output
   
   procedure Bind;
   -- Creates a binder file, waits for the registration, and then executes a
   -- compilation run to compile the binder unit
   
   procedure Expand_Dependencies;
   -- If a main unit is specified, all dependencies are added to the link set,
   -- otherwise the link-set already contains all units. This must be invoked
   -- before Scan_Linker_Options or Link_Or_Archive
   
   procedure Scan_Linker_Options;
   -- Invokes and tracks the Scan_Linker_Options phase when linking an image
   -- or library
   
   procedure Link_Or_Archive with 
     Pre => Command in Build_Command | Run_Command | Library_Command;
   -- Depending on the command (build/run/library), and the output image name
   -- (for library command only), either links an executable, a dynamic library,
   -- or creates a regular object archive (".a" extension)
   
   procedure Execute_Image;
   -- Executes the compiled and linked image, passing no parameters, but
   -- preserving the environment values.
   --
   -- On systems (such as Unix) that support replacing the image of the running
   -- process, Execute_Image will not return. However, on systems that only
   -- support explict process creation (such as Windows), a new process is
   -- created, and Execute_Image returns normally.
   
   procedure Save_Registry;
   -- Saves Last_Run
   
   procedure Save_Config;
   -- Saves the build configuration (parameters)
   
end Scheduling;
