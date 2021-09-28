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

separate (Specification_Scanner)

procedure Scan_Generic
  (Buffer   : in out Ada_Lexical_Parser.Source_Buffer;
   Unit_Tree: in out Declaration_Trees.Tree;
   Root     : in     Declaration_Trees.Cursor)
is
   use Ada_Lexical_Parser;
   use Declaration_Trees;
   package Toolkit is new Parse_Toolkit (Buffer); use Toolkit;
   
   use type WWU.Unbounded_Wide_Wide_String;
   
begin
   Unit_Tree(Root).Is_Generic := True;
   -- Try to keep this as simple as possible, for now we really don't need
   -- this information to generate the configuration extractor programs.
   
   loop
      Skip_To_Semicolon;
      Next_Element;
      exit when Category = Reserved_Word 
        and then Content in "package" | "procedure" | "function";
   end loop;
   
   if Content = "package" then
      Scan_Specification (Buffer    => Buffer,
                          Unit_Tree => Unit_Tree,
                          Root      => Root);
      
   elsif Content in "procedure" | "function" then
      Scan_Subprogram (Buffer          => Buffer,
                       Unit_Tree       => Unit_Tree,
                       Subprogram_Node => Root);
   end if;
end Scan_Generic;
