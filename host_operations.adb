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

with Interfaces.C;

package body Host_Operations is
   
   -------------------
   -- Symbolic_Link --
   -------------------
   
   procedure Symbolic_Link (Target_Path, Source_Path: String) is
      use Interfaces.C;
      
      pragma Linker_Options ("-lc");
      pragma Linker_Options ("host_operations-chmod_meta.o");
      
      function Symlink_Syscall (name1, name2: in char_array) return int with 
        Import => True, Convention => C, External_Name => "symlink";
      -- Imported directly from libc
      
      Ret_Val: int;
   begin
      Ret_Val := Symlink_Syscall (name1 => To_C (Target_Path),
                                  name2 => To_C (Source_Path));
      
      if Ret_Val /= 0 then
         raise Host_Operation_Failed with
           "symlink System Call Failed with errno =" & int'Image (Ret_Val);
      end if;
   end Symbolic_Link;
   
   -------------------
   -- Set_Read_Only --
   -------------------
   
   procedure Set_Read_Only (Target_Path: String) is
      use Interfaces.C;
      
      function Chmod_Meta (path: in char_array) return int with
        Import        => True, 
        Convention    => C,
        External_Name => "__host_operations_chmod_meta";
      -- This is imported from a separate binding since the type "mode_t",
      -- is not standardized, nor are the values of the standard macro values
      
      Ret_Val: int;
   begin
      Ret_Val := Chmod_Meta (To_C (Target_Path));
      
      if Ret_Val /= 0 then
         raise Host_Operation_Failed with
           "stat/chmod System Call Failed with errno =" & int'Image (Ret_Val);
      end if;
   end Set_Read_Only;
   
end Host_Operations;
