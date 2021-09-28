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

-- This is the POSIX (execve) implementation

with Interfaces.C.Strings;

separate (Scheduling)

procedure Execute_Image is
   use Interfaces.C;
   use Interfaces.C.Strings;
   
   Image_Path: constant String := UBS.To_String (Parameters.Executable);
   
   path : constant char_array := To_C (Image_Path);
   arg_1: aliased  char_array := To_C (Ada.Directories.Simple_Name (Image_Path));
   argv : constant chars_ptr_array := (0 => To_Chars_Ptr (arg_1'Unchecked_Access),
                                       1 => Null_Ptr);
   
   environ: access constant chars_ptr_array
     with Convention => C, Import => True, External_Name => "environ";
   -- environ(7)
   
   function execve (path: char_array; argv, envp: chars_ptr_array) return int
   with Convention => C, Import => True, External_Name => "execve";
   -- execve(2)
   
   Discard: int;
begin
   UI.Put_Info_Tag;
   Put_Line (" Running compiled program . . .");
   New_Line;
   UI.Put_Divider;
   
   Discard := execve (path => To_C (Image_Path),
                      argv => argv,
                      envp => environ.all);
   
   -- execve only returns if it failed
   pragma Assert (Discard = -1);
   raise Process_Failed with "execve failed for image at path: " & Image_Path;
   
end Execute_Image;
