------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
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

-- This package provides facilities for querying the PATH (or equivalent)
-- Environment Variable for the presence (and full path) of a program, which
-- can be supplied as an Image_Path

with Ada.Strings.Bounded;

package Child_Processes.Path_Searching is 
   
   Not_In_Path: exception;
   
   function Search_Path (Program: String) return String;
   
   -- Searches all directories in the PATH environment variable for a file 
   -- named Name, to which it returns the full-path.
   --
   -- If the search fails Not_In_Path is raised.
   
   
   type Elaboration_Path_Search (<>) is limited private;
   
   function Initialize (Program: aliased String) 
                       return Elaboration_Path_Search;
   
   -- A Elaboration_Path_Search initialization performs Search_Path for
   -- Program upon and is intended to be used to initialize an
   -- Elaboration_Path_Search object during elaboration of a library package.
   --
   -- The purpose is to allow for elaboration-time search for optional
   -- programs, which may or may not be later invoked by the partition.
   --
   -- Attempting to extract the Path of a Elaboration_Path_Search where this
   -- search fails results in a Not_In_Path exception
   
   function Found (Search: Elaboration_Path_Search) return Boolean;
   
   -- Returns True iff Search.Program was located in PATH, and will thus
   -- implies that a subsequent invocation of Image_Path will be successful
   
   function Image_Path (Search: Elaboration_Path_Search) return String with
     Pre => Found (Search);
   
   -- Returns the full path of Search.Program. If Found (Search) is False,
   -- Not_In_Path is raised
   
private
   
   package Image_Path_Strings is 
     new Ada.Strings.Bounded.Generic_Bounded_Length (256);
   
   type Elaboration_Path_Search (Program: not null access constant String) is
      record
         Result: Image_Path_Strings.Bounded_String;
      end record;

end Child_Processes.Path_Searching;
