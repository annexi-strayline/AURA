------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
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

-- This package coordinates specific queries for the user. Queries are
-- sequential, and causes the invoking task (typically a worker) to block
-- until the query is satisfied

with Ada.Strings.Unbounded;

package User_Queries is
   
   package UBS renames Ada.Strings.Unbounded;
   
   type Query_Stage is 
     (Open,      -- Waiting for Post_Query
      Pending,   -- Waiting for reponse
      Closed);   -- Response available
   
   protected Query_Manager is
      function Query_Active return Boolean;
      -- Indicates True if Query is currently active
      
      entry Start_Query;
      -- Blocks until a query can be made (any active query is complete)
      
      procedure End_Query;
      -- Completes a query, allowing other pending queries to start, if
      -- any. Can be expected not to raise an exception.
      
      procedure Post_Query (Prompt       : in String;
                            Default      : in String;
                            Response_Size: in Positive)
      with Pre => Default'Length = Response_Size;
      -- Raises Program_Error if a query is not active, or a query is still
      -- pending. Response_Size is the maximum size of the response.
      
      entry Wait_Response (Response: out String;
                           Last    : out Natural);
      -- Raises Program_Error if Response is not the size indicated by
      -- Response_Size
      
      function Query_Pending return Boolean;
      -- Returns true if Stage = Pending
      
      procedure Take_Query (Driver: not null access procedure 
                              (Prompt  : in     String;
                               Default : in     String;
                               Response:    out String;
                               Last    :    out Natural));
      -- Invokes Driver for a pending query. If no query is pending,
      -- Program_Error is raised.
      
   private
      Active : Boolean     := False;
      Stage  : Query_Stage := Open;
      
      P_Buffer: UBS.Unbounded_String;
      D_Buffer: UBS.Unbounded_String;
      R_Buffer: UBS.Unbounded_String;
      
   end Query_Manager;
      
end User_Queries;
