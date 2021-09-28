------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
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

-- This package contains the core worker pool which executes queued 
-- "work orders".
--
-- The intent is that "work orders" are used internally within various
-- subsystems, rather than acting as a general calling convention.
--
-- In general, derrivatives of Work_Order should not appear in package
-- specifications (except in private parts or private packages)


with Ada.Containers;

with Progress;

package Workers is
   
   -----------------
   -- Work Orders --
   -----------------
   
   type Work_Order is abstract tagged
      record
         Tracker: Progress.Progress_Tracker_Access := null;
         -- If non-null, the Work_Order will invoke Increment_Completed_Items
         -- on the successful completion of the Order, or
         -- Increment_Failed_Items otherwise
         
      end record;
   -- Any derrivative of the Work_Order interface may be submitted to
   -- Queue_Order for a Worker task to call Execute on the order.
   
   function  Image (Order: Work_Order) return String is abstract; 
   -- Return a string describing the details of the work order, used by
   -- work tasks to create reports when Execute raises an exception.
   
   procedure Execute (Order: in out Work_Order) is abstract;
   -- Dispatching operation to execute the work of the work order. Called by
   -- a worker tasks when executing an order
   
   procedure Phase_Trigger  (Order: in out Work_Order) is null;
   
   -- If Order has a non-null tracker, and at completion of the order
   -- when the tracker is incremented, if the tracker is completed
   -- (Is_Complete) evaluates True, the worker executes Phase_Trigger
   --
   -- The concept for a phase trigger is essentially equivalent to the join
   -- in a fork-join concurrency model.
   --
   -- If Phase_Trigger raises an exception, the worker files a failure report
   -- using image of the triggering order, with a worker note of 
   -- "Phase Trigger failed"
   --
   -- Since Phase_Triggers are often used to schedule (enqueue) more work,
   -- and since that work can sometimes include deallocating concurrent shared
   -- memory for the just completed task, it is important that any work
   -- submitted during a phase trigger is not executed until the phase trigger
   -- completes. Therefore all workers will be held while a phase trigger
   -- executes - again this is conformant with the fork-join model.
   --
   -- Note that the worker will not interact with tracker after executing
   -- an associated Phase_Trigger.
   
   ----------------
   -- Work Queue --
   ----------------
   
   subtype Count_Type is Ada.Containers.Count_Type;
   use type Count_Type;
   
   procedure Enqueue_Order (Order: in Work_Order'Class);
   -- Enqueus a (normal) Work_Order on the work queue.
   
   procedure Defer_Order
     (Order       : in Work_Order'Class;
      Wait_Tracker: not null Progress.Progress_Tracker_Access);
   
   -- Order Deferral
   -- ==============
   --
   -- Each deferred order is dependent on a Progress_Tracker on which the
   -- deferred order is waiting completion.
   --
   -- Deferred orders are intended to act as triggers pending some other
   -- process. It is expected that a given Work Order completes a tracker
   -- on which a deferred work order is waiting. That/those Work Orders
   -- can then be executed. This ensures that the workers are not exhausted
   -- blocking on a progress tracker happening inside of a normal work order.
   --
   -- When submitted, Deferred Orders are enqueued on to the main work queue.
   -- When a worker dequeues a Deferred Order, it immediately checks the
   -- Wait_Tracker for completion. If the tracker indicates completion,
   -- the worker immediately executes the Deferred Order as it would a
   -- regular Work Order. If the tracker does not indicate completion, the
   -- Order is then enqueued on a separate "deferral queue". 
   --
   -- Every worker that completes any work order always drains the deferral
   -- queue back to the main queue, so that each deferred order may then
   -- be re-checked.
   --
   -- The design of this algorithm has a number of features:
   --
   -- *  It ensures that the minimum time is spent re-checking Deferred Orders
   --    that likely have not had their tracker completed, and that the Orders
   --    that release Deferred Orders will tend to be processed before the
   --    related Deferred Order is processed
   --
   -- *  Once the main queue is empty, all workers will wait for new orders to
   --    be enqueued, and will not spin, even if there remains Deferred Orders
   --    that have not been released. This is desireable because, if such
   --    Deferred Orders remained in this case, it should be impossible for
   --    them to complete anyways.
   --
   -- *  In theory, any erroneous Deferred Orders (who's tracker will never
   --    complete) will accumulate on the deferral queue after the main queue
   --    has been empty. This should be easy to spot since a higher-level
   --    process will stall on the Deferred Order, but there will not be
   --    misleading behaviour such as high CPU usage, or random performance
   --    (and potential live lock) due to spinning.
   
   function Queue_Level      return Count_Type;
   function Peak_Queue_Level return Count_Type;
   -- Number of work order in the Queue, or the peak recorded number, not
   -- including deferred orders that have been moved to the deferral queue
   
   function Deferred      return Count_Type;
   function Peak_Deferred return Count_Type;
   -- Number of work orders in the deferral queue
   
   -----------------
   -- Worker Pool --
   -----------------
   
   -- All Workers simply Execute work orders off the Work Queue or Deferral
   -- Queue
   --
   -- Any exception encountered is contained and reported via the failure
   -- reporting facilities
   
   subtype Worker_Count is Natural;

   task type Worker_Task;
   type Worker_Pool is array (Worker_Count range <>) of Worker_Task;
   -- The environment task is expected to initialize an appropriate pool of
   -- workers
   
   procedure Enable_Completion_Reports;
   procedure Disable_Completion_Reports;
   -- If enabled, all workers submit a report on completion of a work order.
   -- This setting is Disabled by default
   
   procedure Wait_Workers;
   procedure Wait_Workers (Timeout: in Duration; Timedout: out Boolean);
   -- Wait until all workers are Idle (no more jobs on the work queue),
   -- or until Timeout expires
   
   procedure Disband_Workers;
   -- Perminantly terminates all worker tasks in the partition. 
   -- This cannot be reversed.
   
   function Busy_Workers      return Worker_Count;
   function Peak_Busy_Workers return Worker_Count;
   -- Total number of Worker tasks currently executing a Work_Order
   
end Workers;
