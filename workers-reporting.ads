------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
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

-- This package contains the worker reporting facility. When the execution of
-- a work order raises an exception, workers will report the circumstances into
-- the reporting queue.

with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

package Workers.Reporting is
   
   New_Line: Character renames Ada.Characters.Latin_1.LF;
   -- Often used by work order Image functions
   
   subtype Report_String is Ada.Strings.Unbounded.Unbounded_String;
   
   function To_Report_String (Source: String) return Report_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   
   function To_String (Source: Report_String) return String
     renames Ada.Strings.Unbounded.To_String;
   
   function Length (Source: Report_String) return Natural
     renames Ada.Strings.Unbounded.Length;
   
   type Report_Kind is (Info, Error);
   
   type Work_Report (Kind: Report_Kind := Info) is
      record
         Work_Order_Information: Report_String;
         -- Result of calling Work_Order'Class.Image of the offending order
         
         Worker_Note: Report_String;
         
         case Kind is
            when Error =>
               Exception_Information : Report_String;
            
            when others =>
               null;
         end case;
         -- Result of calling Ada.Exceptions.Exception_Information on the
         -- encountered exception
         --
         -- Note for debug modes with 
      end record;
   
   procedure Submit_Report (Report: Work_Report);
   
   function  Available_Reports return Count_Type;
   -- Returns the number of reports currently queued
   
   function  Retrieve_Report   return Work_Report;
   -- Returns the next available report. If there are none available,
   -- Retrieve_Report blocks until one is available
   
   
   ------------------
   -- Proxy_Report --
   ------------------
   
   type Proxy_Report is abstract new Work_Order with null record;
   
   -- the Proxy_Report is a general Work_Order that may be submitted by any
   -- process to add reports to the worker reporting queue. These reports are
   -- always of Kind Info.
   --
   -- Upon receipt of a Proxy_Report'Class work order, a worker will generate
   -- and submit a an info report with Work_Order_Information set to whatever
   -- is returned from the Image operation.
   --
   -- The Execute operation of a Proxy_Report is never invoked.
   --
   -- Proxy_Reports are intended for debuging
   
   overriding
   procedure Execute (Order: in out Proxy_Report) is null;
   
   
end Workers.Reporting;
