------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019-2021, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Workers.Reporting;

package body Workers is
   
   use type Progress.Progress_Tracker_Access;
   
   --
   -- Work_Queue
   --
   
   type Order_Holder is access Work_Order'Class;
   
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Object => Work_Order'Class,
      Name   => Order_Holder);
   
   type Queued_Order is
      record
         Holder      : Order_Holder                     := null;
         Wait_Tracker: Progress.Progress_Tracker_Access := null;
      end record;
   
   package SQI is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Queued_Order);
     
   package USQ is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => SQI);
   
   -- The Queues
   Work_Queue    : USQ.Queue;
   Deferral_Queue: USQ.Queue;
   
   -------------------
   -- Enqueue_Order --
   -------------------
   
   procedure Enqueue_Order (Order: in Work_Order'Class) is
   begin
      Work_Queue.Enqueue ((Holder       => new Work_Order'Class'(Order),
                           Wait_Tracker => null));
   end Enqueue_Order;
   
   -----------------
   -- Defer_Order --
   -----------------
   
   procedure Defer_Order
     (Order       : in Work_Order'Class;
      Wait_Tracker: not null Progress.Progress_Tracker_Access) is
   begin
      Work_Queue.Enqueue ((Holder       => new Work_Order'Class'(Order),
                           Wait_Tracker => Wait_Tracker));
   end Defer_Order;
   
   -----------------
   -- Queue_Level --
   -----------------
   
   function Queue_Level return Count_Type is (Work_Queue.Current_Use);
   
   
   ----------------------
   -- Peak_Queue_Level --
   ----------------------
   
   function Peak_Queue_Level return Count_Type is (Work_Queue.Peak_Use);
   
   --------------
   -- Deferred --
   --------------
   
   function Deferred return Count_Type is (Deferral_Queue.Current_Use);
   
   -------------------
   -- Peak_Deferred --
   -------------------
   
   function Peak_Deferred return Count_Type is (Deferral_Queue.Peak_Use);
   
   
   --
   -- Worker Pool
   --
   
   ----------------
   -- Accounting --
   ----------------
   
   protected Worker_Accounting is
      entry     Start_Job;
      procedure Finish_Job;
      
      entry     Wait_Idle;
      
      function  Busy_Workers  return Worker_Count;
      function  Peak_Busy_Workers return Worker_Count;
      
      procedure Set_Completion_Reports (New_Setting: in Boolean);
      function  Completion_Reporting return Boolean;
      
      entry     Phase_Trigger_Lock;
      procedure Phase_Trigger_Release;
      
      -- When the Phase_Trigger is active, no workers will accept new
      -- jobs. This ensures that any work scheduled during a phase
      -- trigger cannot be finished before the phase trigger finished.
      --
      -- In a sense, the phase trigger lock is used to ensure a strict
      -- "fork-join" behaviour expected by the phase trigger feature
      
   private
      Busy_Count          : Worker_Count := 0;
      Busy_Peak           : Worker_Count := 0;
      Completion_Reports  : Boolean      := False;
      Phase_Trigger_Active: Boolean      := False;
   end Worker_Accounting;
   
   
   protected body Worker_Accounting is
      
      ---------------
      -- Start_Job --
      ---------------
      
      entry Start_Job when not Phase_Trigger_Active is
      begin
         Busy_Count := Busy_Count + 1;
         
         if Busy_Count > Busy_Peak then
            Busy_Peak := Busy_Count;
         end if;
      end Start_Job;
      
      ----------------
      -- Finish_Job --
      ----------------
      
      procedure Finish_Job is
      begin
         Busy_Count := Busy_Count - 1;
      end Finish_Job;
      
      ---------------
      -- Wait_Idle --
      ---------------
      
      entry Wait_Idle
        when Busy_Count = 0 is
      begin
         null;
      end Wait_Idle;
      
      ---------------
      -- Statitics --
      ---------------
      
      function  Busy_Workers      return Worker_Count is (Busy_Count);
      function  Peak_Busy_Workers return Worker_Count is (Busy_Peak);
   
      --------------------------
      -- Completion Reporting --
      --------------------------
      
      procedure Set_Completion_Reports (New_Setting: in Boolean) is
      begin
         Completion_Reports := New_Setting;
      end Set_Completion_Reports;
      
      function Completion_Reporting return Boolean is (Completion_Reports);
      
      ---------------------------
      -- Phase_Trigger Locking --
      ---------------------------
   
      entry Phase_Trigger_Lock when not Phase_Trigger_Active is
      begin
         Phase_Trigger_Active := True;
      end Phase_Trigger_Lock;
      
      procedure Phase_Trigger_Release is
      begin
         Phase_Trigger_Active := False;
      end Phase_Trigger_Release;
   
   end Worker_Accounting;
   
   -----------------------
   -- Termination_Order --
   -----------------------
   
   type Termination_Order is new Work_Order with null record;
   -- Termination orders cause the targeted worker task to terminate
   
   overriding
   function Image (Order: Termination_Order) return String is
     (raise Program_Error);
   
   overriding 
   procedure Execute (Order: in out Termination_Order) is
   begin
      raise Program_Error;
   end Execute;
   
   -----------------
   -- Worker_Task --
   -----------------
   
   task body Worker_Task is
      
      function To_Report_String (Source: String) 
                                return Reporting.Report_String
        renames Reporting.To_Report_String;
      
      function Exception_Info (X: Ada.Exceptions.Exception_Occurrence) 
                              return String
        renames Ada.Exceptions.Exception_Information;
      
      Dequeued_Order: Queued_Order;
      Assigned_Order: Order_Holder renames Dequeued_Order.Holder;
      
      Busy: Boolean := False;
      
      procedure Start_Job is
      begin 
         Worker_Accounting.Start_Job;
         Busy := True;
      end Start_Job;
      
      procedure Finish_Job is
      begin
         Worker_Accounting.Finish_Job;
         Busy := False;
      end Finish_Job;
      
      procedure Get_Next_Job is
         -- A roll-up procedure to obtain a new work order and change the
         -- busy state only as needed
      begin
         
         -- First drain the deferral queue
         loop
            select
               Deferral_Queue.Dequeue (Dequeued_Order);
            else
               exit;
            end select;
            
            Work_Queue.Enqueue (Dequeued_Order);
         end loop;
         
         
         -- Now grab an order
         loop
            
            select
               Work_Queue.Dequeue (Dequeued_Order);
            else
               -- Nothing is available right now
               -- If we're busy lets inciate that we're now idle while we wait
               if Busy then
                  Finish_Job;
               end if;
               Work_Queue.Dequeue (Dequeued_Order);
            end select;
            
            -- If the order is a deferral order, it needs to be requeued on
            -- the deferral queue if it is not ready to execute yet
            if Dequeued_Order.Wait_Tracker /= null
              and then not Dequeued_Order.Wait_Tracker.Is_Complete
            then
               -- Not ready. Off to the deferral queue and try again
               Deferral_Queue.Enqueue (Dequeued_Order);
               
            else
               exit;
            end if;
         end loop;
         
         if not Busy then
            Start_Job;
            
            -- We will be held here if another worker is currently executing
            -- any Phase_Trigger
         end if;
         
      end Get_Next_Job;
      
      Phase_Trigger_Armed: Boolean;
      
   begin
      -- Wait for our very first job
      Get_Next_Job;
      
      loop
         Phase_Trigger_Armed := False;
         
         -- Termination orders
         if Assigned_Order.all in Termination_Order then
            -- Requeue for all other workers
            Work_Queue.Enqueue (Dequeued_Order);
            Finish_Job;
            exit;
            
         -- Proxy reports
         elsif Assigned_Order.all in Reporting.Proxy_Report'Class then
            declare
               Proxy_Report: Reporting.Work_Report (Reporting.Info);
            begin
               Proxy_Report.Work_Order_Information := To_Report_String
                 (Assigned_Order.Image);
               
               Proxy_Report.Worker_Note := To_Report_String
                 ("<<Proxy Report>>");
               
               Reporting.Submit_Report (Proxy_Report);
            end;
            
         -- Normal work orders
         else
            declare
               use type Progress.Progress_Tracker_Access;
            begin
               Assigned_Order.Execute;
               
               if Assigned_Order.Tracker /= null then
                  Assigned_Order.Tracker.Increment_Completed_Items 
                    (Completed => Phase_Trigger_Armed);
               end if;
               
            exception
               when e: others => 
                  if Assigned_Order.Tracker /= null then
                     Assigned_Order.Tracker.Increment_Failed_Items
                       (Completed => Phase_Trigger_Armed);
                  end if;
                  
                  declare
                     Report: Reporting.Work_Report (Reporting.Error);
                  begin
                     Report.Work_Order_Information := To_Report_String 
                       (Assigned_Order.Image);
                     
                     Report.Exception_Information := To_Report_String 
                       (Exception_Info (e));
                     
                     Reporting.Submit_Report (Report);
                  end;
            end;
            
            -- Execute the Phase_Trigger operation if this was the last order
            -- of a tracker
            if Phase_Trigger_Armed then
               Phase_Trigger_Armed := False;
               Worker_Accounting.Phase_Trigger_Lock;
               
               begin
                  Assigned_Order.Phase_Trigger;
               exception
                  when e: others =>
                     declare
                        Report: Reporting.Work_Report (Reporting.Error);
                     begin
                        Report.Work_Order_Information := To_Report_String 
                          (Assigned_Order.Image);
                        
                        Report.Exception_Information := To_Report_String 
                          (Exception_Info (e));
                        
                        Report.Worker_Note := To_Report_String 
                          ("Phase Trigger failed");
                        
                        Reporting.Submit_Report (Report);
                  end;
               end;
               
               Worker_Accounting.Phase_Trigger_Release;
            end if;
            
            -- File a completion report if enabled
            if Worker_Accounting.Completion_Reporting then
               declare
                  Completion_Report: Reporting.Work_Report (Reporting.Info);
               begin
                  Completion_Report.Work_Order_Information:= To_Report_String 
                    (Assigned_Order.Image);
                  
                  Completion_Report.Worker_Note := To_Report_String 
                    ("Work order completed");
                  
                  Reporting.Submit_Report (Completion_Report);
               end;
            end if;
         end if;
         
         Deallocate (Assigned_Order);
         Get_Next_Job;
      end loop;
      
   exception
      when e: others =>
         declare
            Report: Reporting.Work_Report (Reporting.Error);
         begin
            Report.Work_Order_Information := To_Report_String 
              ("<<GENERAL WORKER FAILURE>>");
            
            Report.Worker_Note := To_Report_String 
              ("Worker task experienced an unhandled exception");
            
            Report.Exception_Information := To_Report_String 
              (Exception_Info (e));
            
            Reporting.Submit_Report (Report);
            
            if Busy then
               Finish_Job;
            end if;
         end;
   end Worker_Task;
   
   ------------------------
   -- Completion Reports --
   ------------------------
   
   procedure Enable_Completion_Reports is 
   begin
      Worker_Accounting.Set_Completion_Reports (True);
   end Enable_Completion_Reports;
   
   procedure Disable_Completion_Reports is
   begin
      Worker_Accounting.Set_Completion_Reports (False);
   end Disable_Completion_Reports;
   
   ------------------
   -- Wait_Workers --
   ------------------
   
   procedure Wait_Workers is
   begin
      Worker_Accounting.Wait_Idle;
   end Wait_Workers;
   
   --------------------------------------------------
   procedure Wait_Workers (Timeout: in Duration; Timedout: out Boolean) is
   begin
      select
         Worker_Accounting.Wait_Idle;
         Timedout := False;
      or
         delay Timeout;
         Timedout := True;
      end select;
   end Wait_Workers;
   
   ---------------------
   -- Disband_Workers --
   ---------------------
   
   procedure Disband_Workers is
   begin
      -- We simply insert a single Termination_Order on the queue, which
      -- will remain there
      
      Work_Queue.Enqueue ((Holder       => new Termination_Order,
                           Wait_Tracker => null));
      
   end Disband_Workers;
   
   ------------------
   -- Busy_Workers --
   ------------------
   function Busy_Workers return Worker_Count 
     is (Worker_Accounting.Busy_Workers);
   
   -----------------------
   -- Peak_Busy_Workers --
   -----------------------
   
   function Peak_Busy_Workers return Worker_Count
     is (Worker_Accounting.Peak_Busy_Workers);
   
end Workers;
