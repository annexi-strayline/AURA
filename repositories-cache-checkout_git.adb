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

with Ada.Streams;
with Ada.Assertions;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Bounded;

with Child_Processes;
with Child_Processes.Path_Searching;
with Child_Processes.Wait_And_Buffer;

separate (Repositories.Cache)

package body Checkout_Git is
   
   Git_Cache_Failure: exception;
   
   package Program_Paths renames Child_Processes.Path_Searching;
   
   Git_Program     : aliased constant String := "git";
   Git             : constant Program_Paths.Elaboration_Path_Search
     := Program_Paths.Initialize (Git_Program);
   Git_Timeout     : constant Duration := 300.0; -- 5 minutes
   Output_Poll_Rate: constant Duration := 0.5;
   
   package Output_Buffers is 
     new Ada.Strings.Bounded.Generic_Bounded_Length (8096);
   
   procedure Buffer_Append (Buffer: in out Output_Buffers.Bounded_String;
                            Item  : in     String)
   is begin
      -- Since we are using bounded inputs, we'd rather not crash just because
      -- the input exceeds the buffers, but rather have it truncate.
      
      Output_Buffers.Append (Source   => Buffer,
                             New_Item => Item,
                             Drop     => Ada.Strings.Right);
   end Buffer_Append;
   
   procedure Wait_And_Buffer is new Child_Processes.Wait_And_Buffer 
     (Buffer_Type  => Output_Buffers.Bounded_String, 
      Append       => Buffer_Append,
      Empty_Buffer => Output_Buffers.Null_Bounded_String);
                             
   --
   -- Checkout_Git_Order
   --
   
   -- Checkout_Order checks out a git repository when the cache does
   -- not exist (or has been invalidated)
   
   
   
   type Cache_Git_Order is new Workers.Work_Order with
      record
         Repo : Repository;
         Index: Repository_Index;
      end record;
   
   overriding procedure Execute (Order: in out Cache_Git_Order);
   overriding function  Image   (Order: Cache_Git_Order) return String;
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Cache_Git_Order) return String
     is (    "[Cache_Git_Order]" & New_Line 
           & " Repoistory No."
           & Repository_Index'Image (Order.Index) & New_Line
           & " Git   : " & UBS.To_String (Order.Repo.Location)
           & (if (UBS.Length (Order.Repo.Snapshot) > 0) then
                 New_Line & " Commit: " & UBS.To_String (Order.Repo.Snapshot)
              else
                 "")
           & (if (UBS.Length (Order.Repo.Tracking_Branch) > 0) then
                 New_Line & " Branch: " 
                 & UBS.To_String (Order.Repo.Tracking_Branch)
              else
                 ""));
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Cache_Git_Order) is
      use Child_Processes;
      use type Ada.Directories.File_Kind;
      
      Trimmed_Index: constant String
        := Ada.Strings.Fixed.Trim 
          (Source => Repository_Index'Image (Order.Index),
           Side   => Ada.Strings.Both);
      
      Cache_Path: constant String := Cache_Root & '/' & Trimmed_Index;
      
      STDERR: Output_Buffers.Bounded_String;
      STDOUT: Output_Buffers.Bounded_String;
      
      procedure Wait_Proc (Title: in String;
                           Proc : in out Child_Process'Class)
      is 
         type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
         
         Timed_Out: Boolean;
         Proc_Exit: Exit_Status := Failure;
      begin
         -- Wait until finish, if timeout, kill the process. If
         -- exit status = Failure then drain Standard_Error. In both cases,
         -- raise an exception
         
         Wait_And_Buffer (Process   => Proc,
                          Poll_Rate => Output_Poll_Rate,
                          Timeout   => Git_Timeout,
                          Output    => STDOUT,
                          Error     => STDERR,
                          Timed_Out => Timed_Out,
                          Status    => Proc_Exit);
         
         if Timed_Out then
            -- This should actually be very rare
            Proc.Kill;
            raise Git_Cache_Failure with Title 
              & " timed-out" 
              & " STDOUT: " & Output_Buffers.To_String (STDOUT)
              & " STDERR: " & Output_Buffers.To_String (STDERR);
            
         elsif Proc_Exit = Failure then
               
            raise Git_Cache_Failure with Title & " failed: " 
              & Output_Buffers.To_String (STDERR);
         end if;
         
      end Wait_Proc;
      
      
      procedure Git_Command (Args       : in String;
                             Ignore_Fail: Boolean := False) is
         Proc: Child_Process'Class 
           := Spawn_Process
             (Image_Path        => Program_Paths.Image_Path (Git),
              Arguments         => Args,
              Working_Directory => Cache_Path);
      begin
         Wait_Proc (Title => "git " & Args, 
                    Proc  => Proc);
         
      exception
         when Git_Cache_Failure =>
            if Ignore_Fail then
               return;
            else
               raise;
            end if;
      end Git_Command;
      
      Cache_Bad: Boolean := False; 
      
   begin
      UBS.Set_Unbounded_String (Target => Order.Repo.Cache_Path,
                                Source => Cache_Path);
      Order.Repo.Cache_State := Available;
      -- These will only be updated in the registry if the checkout is
      -- successful
      
      
      -- Verify that git is actually available
      if not Program_Paths.Found (Git) then
         raise Program_Error with
           "Unable to checkout git repository: Could not find git!";
      end if;
      

      -- First step is to see if this repo is already cached, and if it is,
      -- that it is on the right commit or branch, and that it has no
      -- unstaged/uncommited changes. If that is all good, we're done. If any
      -- of that is off, we wipe it and start from scratch (the user should
      -- never be messing with a git cache directly)
      
      if Ada.Directories.Exists (Cache_Path) then
         if Ada.Directories.Kind (Cache_Path) = Ada.Directories.Directory then

            begin
               
               if UBS.Length (Order.Repo.Snapshot) > 0 then
                  -- If Snapshot is not empty, this means the repo is on a
                  -- specific commit (regardless of any Tracking_Branch).
                  
                  Git_Command ("log -1 --pretty=format:%H");
                  
                  if Output_Buffers.To_String (STDOUT) 
                    /= UBS.To_String (Order.Repo.Snapshot)
                  then
                     Cache_Bad := True;
                  end if;
               end if;
               
               -- Check for any pollution. We expect a short status to only
               -- have one line, giving either "## HEAD" (Snapshot) or
               --"## branch...origin/branch" (Tracking_Branch)
               --
               -- Any lines following that indicate changes to the cache, which
               -- is not acceptable
               
               Git_Command ("status -s -b");
               
               -- If we have a snapshot, we expect a detached head
               if UBS.Length (Order.Repo.Snapshot) > 0 
                 -- "## HEAD "
                 --  ^      ^
                 --  1      8
                 
                 and then Output_Buffers.Slice (Source => STDOUT,
                                                Low    => 1,
                                                High   => 8)
                 /= "## HEAD "
               then
                  Cache_Bad := True;
                  
               else
                  -- If we don't have a snapshot, we expect to see the correct
                  -- branch
                  
                  pragma Assert (UBS.Length (Order.Repo.Tracking_Branch) > 0);
                  -- This should be checked when the repo file is parsed
                  
                  declare
                     Expected: constant String
                       := "## " 
                         & UBS.To_String (Order.Repo.Tracking_Branch)
                         & "...origin/"
                         & UBS.To_String (Order.Repo.Tracking_Branch);
                  begin
                     if Output_Buffers.Slice (Source => STDOUT,
                                              Low    => 1,
                                              High   => Expected'Length)
                       /= Expected
                     then
                        Cache_Bad := True;
                     end if;
                  end;
               end if;
               
               
               -- Finally, there should be only one line
               if Output_Buffers.Count (Source  => STDOUT,
                                        Pattern => String'(1 .. 1 => New_Line))
                 > 1
               then
                  Cache_Bad := True;
               end if;
               
            exception
               when Git_Cache_Failure =>
                  -- Any of the above commands failing simply means the cache
                  -- is invalid, so we just need to destroy it
                  Cache_Bad := True;
            end;
            
            if not Cache_Bad then
               -- done.
               Update_Repository (Index   => Order.Index,
                                  Updated => Order.Repo);
               return;
            else
               -- Expunge
               Ada.Directories.Delete_Tree (Cache_Path);
            end if;
            
         else
            -- Path points at a non-directory file, that's not ok
            Ada.Directories.Delete_File (Cache_Path);
            
         end if;

      end if;
      
      -- If we get here, then either the cache didn't exist, or something was
      -- wrong with it and it was obliterated
      
      -- Create the cache path
      Ada.Directories.Create_Path (Cache_Path);
      
      
      -- Clone the repo
      Git_Command ("clone -n -q -j 0 " -- Quiet and don't checkout anything
                     &  UBS.To_String (Order.Repo.Location) & ' '
                     &  Cache_Path);
      
      
      -- If the snapshot is set, that takes priority
      if UBS.Length (Order.Repo.Snapshot) > 0 then
         Git_Command ("checkout --detach " 
                        & UBS.To_String (Order.Repo.Snapshot));
         
      -- If Tracking_Branch is set, and we have no snapshow, check that out
      elsif UBS.Length (Order.Repo.Tracking_Branch) > 0 then

         Git_Command ("checkout " 
                        & UBS.To_String (Order.Repo.Tracking_Branch));
         
      -- Otherwise, we just use the default branch
      else
         Git_Command ("checkout "
                        & UBS.To_String (Order.Repo.Tracking_Branch));
         
      end if;
      
      -- Finally we checkout submodules, if any
      -- TODO: Add a non-zero -j value (git regression), once user
      -- -j switch is added to AURA CLI
      Git_Command ("submodule --quiet update "
                     & "--init --checkout --recursive");
      
      Update_Repository (Index   => Order.Index,
                         Updated => Order.Repo);
      
   end Execute;
   
   
   --------------
   -- Dispatch --
   --------------
   
   procedure Dispatch (Repo: in Repository; Index: in Repository_Index) is
      Order: Cache_Git_Order
        := (Tracker => Caching_Progress'Access,
            Repo    => Repo,
            Index   => Index);
   begin
      Workers.Enqueue_Order (Order);
   end Dispatch;
   
   
end Checkout_Git;
