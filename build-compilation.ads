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

with Ada.Strings.Unbounded;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

with Progress;
with Registrar.Library_Units;

package Build.Compilation is
   
   type Compilation_Message_Context is (Warning, Error);
   
   type Compiler_Message is
      record
         Unit   : Registrar.Library_Units.Library_Unit;
         Context: Compilation_Message_Context;
         STDERR : UBS.Unbounded_String;
      end record;
   
   function Get_Context (Message: Compiler_Message) 
                        return Compilation_Message_Context is 
     (Message.Context);
   
   function Before (Left, Right: Compilation_Message_Context) return Boolean is
     (Left = Error and then Right = Warning);
   
   -- We want to dequeue Errors before Warnings
   
   package CMQI is new Ada.Containers.Synchronized_Queue_Interfaces
     (Compiler_Message);
   
   package CMQ is new Ada.Containers.Unbounded_Priority_Queues
     (Queue_Interfaces => CMQI,
      Queue_Priority   => Compilation_Message_Context,
      Get_Priority     => Get_Context,
      Before           => Before);
   
   -------------
   -- Compile --
   -------------
   
   procedure Compile (Configuration: in Build_Configuration);
   Compilation_Progress: aliased Progress.Progress_Tracker;
   Compiler_Messages   : CMQ.Queue;
   
   -- Attempts to compile all units with a State of "Available".
   --
   -- Any options in Global_Compiler_Options are prepended to any of the
   -- computed options determined by the subsystem/root configuration
   --
   -- Compilation output is always output to files at Compiler_Output (see
   -- private part)
   -- * unit_name.cmd: The command (and arguments) used to invoke the compiler
   -- * unit_name.out: The standard output stream of the compiler
   -- * unit_name.err: The standard error stream of the compiler
   --
   -- Additionally, std_err is output to the Compiler_Messages queue if it
   -- is non-empty.
   --
   -- If the compiler process exits successfully, all stderr output is assumed
   -- to be warning messages, and Context is set to Warnings. Otherwise,
   -- Context is set to Error on a non-successful exit status
   --
   -- Compiled units will have ther hashes regenerated, and their state
   -- promoted to "Compiled"

end Build.Compilation;
