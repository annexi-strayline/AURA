------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

package body Progress is
   
   --
   -- Progress_Tracker
   --
   
   protected body Progress_Tracker is
      
      -----------
      -- Reset --
      -----------
      
      procedure Reset is
      begin
         Total     := 0;
         Completed := 0;
         Failed    := 0;
      end Reset;
      
      -----------------
      -- Total_Items --
      -----------------
      
      function Total_Items return Natural is (Total);
   
      ---------------------
      -- Set_Total_Items --
      ---------------------
      
      procedure Set_Total_Items (Items: in Natural)
      is begin
         Total := Items;
      end Set_Total_Items;
      
      ---------------------------
      -- Increment_Total_Items --
      ---------------------------
      
      procedure Increment_Total_Items is 
      begin
         Total := Total + 1;
      end Increment_Total_Items;
      
      -----------------------------
      -- Increase_Total_Items_By --
      -----------------------------
      
      procedure Increase_Total_Items_By (Items: in Natural) is
      begin
         Total := Total + Items;
      end Increase_Total_Items_By;
      
      --------------
      -- Wait_Set --
      --------------
      
      entry Wait_Set when Total > 0 is begin null; end;
      
      ---------------------
      -- Completed_Items --
      ---------------------
      
      function Completed_Items return Natural is (Completed);
   
      -------------------------
      -- Set_Completed_Items --
      -------------------------
      
      procedure Set_Completed_Items (Items: in Natural)
      is begin
         Completed := Items;
      end Set_Completed_Items;
      
      -------------------------------
      -- Increment_Completed_Items --
      -------------------------------
      
      procedure Increment_Completed_Items is 
      begin
         Completed := Completed + 1;
         pragma Assert (Completed + Failed <= Total);
      end Increment_Completed_Items;
      
      --------------------------------------------------
      procedure Increment_Completed_Items (Completed: out Boolean) is 
      begin
         Increment_Completed_Items;
         Completed := Is_Complete;
      end Increment_Completed_Items;
      
      ----------------------
      -- Percent_Complete --
      ----------------------
      
      function Percent_Complete return Percent
      is begin
         if Total = 0 then
            return 0;
         else
            return (Percent ((Float(Completed) / Float(Total)) * 100.0));
         end if;
      end Percent_Complete;
      
      -----------------
      -- Is_Complete --
      -----------------
      
      function Is_Complete return Boolean is (Completed + Failed = Total);
      
      -------------------
      -- Wait_Complete --
      -------------------
      
      entry Wait_Complete when Is_Complete is begin null; end;
      
      ------------------
      -- Failed_Items --
      ------------------
      
      function Failed_Items return Natural is (Failed);
      
      ----------------------
      -- Set_Failed_Items --
      ----------------------
      
      procedure Set_Failed_Items (Items: in Natural) is
      begin
         Failed := Items;
      end Set_Failed_Items;
      
      ----------------------------
      -- Increment_Failed_Items --
      ----------------------------
      
      procedure Increment_Failed_Items is
      begin
         Failed := Failed + 1;
         pragma Assert (Completed + Failed <= Total);
      end Increment_Failed_Items;
   
      --------------------------------------------------
      procedure Increment_Failed_Items (Completed: out Boolean) is
      begin
         Increment_Failed_Items;
         Completed := Is_Complete;
      end Increment_Failed_Items;
   
   end Progress_Tracker;
   
   --
   -- Sequence_Beacon
   --
   
   protected body Sequence_Beacon is
      
      function Active return Boolean is (Active_Aspect);
      
      procedure Approach (Go: out Boolean) is
      begin
         if Active_Aspect then
            Go := False;
         else
            Active_Aspect := True;
            Go := True;
         end if;
      end Approach;
   
      procedure Leave is
      begin
         if Active_Aspect then
            Active_Aspect := False;
         else
            raise Program_Error with 
              "Attempt to Leave inactive Progress_Beacon";
         end if;
      end Leave;
   
      entry Wait_Leave when not Active is
      begin
         null;
      end Wait_Leave;
   
   end Sequence_Beacon;
   
end Progress;
