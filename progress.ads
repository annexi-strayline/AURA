------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019-2022, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- This package contains a universal tracking object that allows for the
-- real-time progress tracking of work-orders

package Progress is
   
   ----------------------
   -- Progress_Tracker --
   ----------------------
   
   type Percent is range 0 .. 100;
   
   protected type Progress_Tracker is
      
      procedure Reset;
      
      -- Sets Total, Completed, and Failures to zero
      
      
      -----------------
      -- Total_Items --
      -----------------
      
      function Total_Items return Natural;
      
      
      procedure Set_Total_Items (Items: in Natural);
      
      
      function Remaining_Items return Natural;
      
      
      procedure Increment_Total_Items;
      
      
      procedure Increase_Total_Items_By (Items: in Natural);
      
      -- Adds Item to the current total
      
      
      entry     Wait_Set;
      
      -- Waits unil Total_Items > 0
      
      
      ---------------------
      -- Completed_Items --
      ---------------------
      
      function Completed_Items return Natural;
      
      
      procedure Set_Completed_Items (Items: in Natural);
      
      
      procedure Increment_Completed_Items;
      
      
      procedure Increment_Completed_Items (Completed: out Boolean);
      
      -- If the incrementation causes the tracker to complete,
      -- Completed is True, otherwise False.
      
      
      function Percent_Complete return Percent;
      
      
      function Is_Complete return Boolean;
      
      -- True if Completed + Failed = Total
      
      
      entry    Wait_Complete;
      
      -- Wait until Is_Complete is True
      
      
      ------------------
      -- Failed_Items --
      ------------------
      
      function  Failed_Items return Natural;
      
      
      procedure Set_Failed_Items (Items: in Natural);
      
      
      procedure Increment_Failed_Items;
      
      
      procedure Increment_Failed_Items (Completed: out Boolean);
      
      -- If the incrementation causes the tracker to complete,
      -- Completed is True, otherwise False.
      
      
      procedure Fail_All_Remaining;
      
      -- Causes Failed_Items to be set to the sum of Completed_Items and
      -- Failed_Items, subtracted from Total_Items. Useful when processes need to abort.
      
      
   private
      
      Total    : Natural := 0;
      Completed: Natural := 0;
      Failed   : Natural := 0;
      
   end Progress_Tracker;
   
   type Progress_Tracker_Access is access all Progress_Tracker;
   
   ---------------------
   -- Sequence_Beacon --
   ---------------------
   
   -- Basically a mutex
   
   protected type Sequence_Beacon is
      function Active return Boolean;
      -- Returns the current generation of the checkpoint
      
      procedure Approach (Go: out Boolean);
      -- Attempt to occupy (activate) the beacon.
      -- If the beacon is already activated, Go is False,
      -- otherwise the beacon is activated and Go is True
      
      procedure Leave;
      -- Deactivate the beacon.
      -- Program_Error is raised if not currently active
      
      entry Wait_Leave;
      -- If the Beacon is Active, block until it is left
      
   private
      Active_Aspect: Boolean := False;
      
   end Sequence_Beacon;
   
end Progress;
