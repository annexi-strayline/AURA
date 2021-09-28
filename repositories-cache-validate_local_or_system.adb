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

with Ada.Assertions;
with Ada.Directories;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;

with Unit_Names;
with User_Queries;
with Stream_Hashing.Collective;
with Registrar.Library_Units;
with Registrar.Queries;

separate (Repositories.Cache)

package body Validate_Local_Or_System is
   
   type Hash_Queue_Access is 
     access Stream_Hashing.Collective.Hash_Queues.Queue;
   
   --
   -- Hash_File_Order
   --
   
   type Hash_File_Order is new Workers.Work_Order with
      record
         Path  : UBS.Unbounded_String;
         Hashes: Hash_Queue_Access;
         
         -- The following is along for the ride - it will be used to
         -- fill-out a Validate_Local_Or_System_Order in the Phase_Trigger
         Index : Repository_Index;
      end record;
   
   overriding procedure Execute       (Order: in out Hash_File_Order);
   overriding procedure Phase_Trigger (Order: in out Hash_File_Order);
   overriding function  Image         (Order: Hash_File_Order) return String;
   
   
   --
   -- Validate_Local_Or_System_Order
   --
   
   -- This order is actually called from the last Hash_File_Order, and
   -- generates the collective hash, and then updates/validates the repo
   
   type Validate_Local_Or_System_Order is new Workers.Work_Order with
      record
         Index : Repository_Index;
         Hashes: Hash_Queue_Access;
      end record;
   
   overriding procedure Execute (Order: in out Validate_Local_Or_System_Order);
   overriding function  Image   (Order: Validate_Local_Or_System_Order)
                                return String;
   
   --
   -- Hash_File_Order
   --
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Hash_File_Order) return String
     is (    "[Hash_File_Order] (Repositories.Cache.Validate_Local_Or_System)" 
           & New_Line 
           & " Path: " & UBS.To_String (Order.Path) & New_Line
           & " (For validation/generation of Repository No."
           & Repository_Index'Image (Order.Index) & ')');
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Hash_File_Order) is
      use Ada.Streams.Stream_IO;
      
      File: File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => UBS.To_String (Order.Path));
      
      Order.Hashes.Enqueue
        (Stream_Hashing.Digest_Stream (Stream (File)));
      
      Close (File);
      
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         
         raise;
   end Execute;
   
   -------------------
   -- Phase_Trigger --
   -------------------
   
   -- This is executred one, when all Hash_Orders have been completed. We can
   -- safely deallocate the dynamic tracker, and then submit a
   -- Validate_Local_Or_System_Order, which handles the rest
   
   procedure Phase_Trigger (Order: in out Hash_File_Order) is

      
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Progress.Progress_Tracker,
         Name   => Progress.Progress_Tracker_Access);
      
      Next_Phase: Validate_Local_Or_System_Order
        := (Tracker => Caching_Progress'Access,
            -- Note that Total_Items was incremented on the call to
            -- Dispatch that got the whole proverbial ball rolling
            
            Index   => Order.Index,
            Hashes  => Order.Hashes);
   begin
      Free (Order.Tracker);
      Workers.Enqueue_Order (Next_Phase);
      
   end Phase_Trigger;
   
   
   --
   -- Validate_Local_Or_System_Order 
   --
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Validate_Local_Or_System_Order) return String
     is (    "[Validate_Local_Or_System_Order] "
           & "(Repositories.Cache.Validate_Local_Or_System)" 
           & New_Line
           & " Repository No." 
           & Repository_Index'Image (Order.Index));
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Validate_Local_Or_System_Order) is
      use Stream_Hashing;
      use Stream_Hashing.Collective;
      
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Hash_Queues.Queue,
         Name   => Hash_Queue_Access);
      
      Repo: Repository := Extract_Repository (Order.Index);
      
      Collect_Hash: Hash_Type;
      Hash_String : UBS.Unbounded_String;
      
   begin
      pragma Assert (Repo.Format in System | Local);
      pragma Assert (Repo.Cache_State = Requested);
      
      Compute_Collective_Hash (Hash_Queue      => Order.Hashes.all,
                               Collective_Hash => Collect_Hash);
      
      -- Release the queue
      Free (Order.Hashes);
      
      UBS.Set_Unbounded_String (Target => Hash_String,
                                Source => Collect_Hash.To_String);
      
      if UBS.Length (Repo.Snapshot) = 0 then
         -- New repo
         Repo.Snapshot    := Hash_String;
         Repo.Cache_State := Available;
         Repo.Cache_Path  := Repo.Location;
         Update_Repository (Index   => Order.Index,
                            Updated => Repo);
         Generate_Repo_Spec (Order.Index);
         return;
         
      else
         -- Verify the hash
         if Collect_Hash.To_String /= UBS.To_String (Repo.Snapshot) then
            -- Mismatch. Ask the user if they want to proceed (use the new
            -- hash), or abort
            
            declare
               use User_Queries;
               Response: String (1..1);
               Last: Natural;
            begin
               Query_Manager.Start_Query;
               
               loop
                  Query_Manager.Post_Query
                    (Prompt => "Repository" 
                       & Repository_Index'Image (Order.Index)
                       & '(' & UBS.To_String (Repo.Location) & ')'
                       & " has been modified. Accept changes? (Y/[N]): ",
                     Default       => "N",
                     Response_Size => Response'Length);
                  
                  Query_Manager.Wait_Response (Response => Response,
                                               Last     => Last);
                  
                  if Last < Response'First then
                     -- Default (N)
                     Response := "N";
                  end if;
                  
                  exit when Response in "Y" | "y" | "N" | "n";
               end loop;
               
               Query_Manager.End_Query;
               
               if Response in "Y" | "y" then
                  -- Update with new hash
                  Repo.Snapshot    := Hash_String;
                  Repo.Cache_State := Available;
                  Repo.Cache_Path  := Repo.Location;
                  Update_Repository (Index   => Order.Index,
                                     Updated => Repo);
                  Generate_Repo_Spec (Order.Index);
               else
                  -- Abort
                  raise Ada.Assertions.Assertion_Error with
                    "Repository hash mismatch. User rejected changes.";
                  
               end if;
            end;
               
         else
            -- Hash ok
            Repo.Cache_State := Available;
            Repo.Cache_Path  := Repo.Location;
            Update_Repository (Index   => Order.Index,
                               Updated => Repo);
         end if;
      end if;
      
   end Execute;
   
   --------------
   -- Dispatch --
   --------------
   
   procedure Dispatch (Repo: in Repository; Index: in Repository_Index) is
      use Ada.Directories;
      use type Ada.Containers.Count_Type;
      
      package Path_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => UBS.Unbounded_String,
         "="          => UBS."=");
      
      Path_Vector: Path_Vectors.Vector;
      
      Group_Tracker: Progress.Progress_Tracker_Access 
        := new Progress.Progress_Tracker;
      
      New_Order: Hash_File_Order 
        := (Tracker => Group_Tracker,
            Hashes  => new Stream_Hashing.Collective.Hash_Queues.Queue,
            Index   => Index,
            others  => <>);
      
      Filter: constant Filter_Type := (Directory     => True,
                                       Ordinary_File => True,
                                       Special_File  => False);
      
      procedure Recursive_Add (E: in Directory_Entry_Type);
      
      procedure Recursive_Add (E: in Directory_Entry_Type) is
         S_Name: constant String := Simple_Name (E);
      begin
         -- Don't process hidden files
         if S_Name(S_Name'First) = '.' then
            return;
         end if;
         
         if Kind (E) = Directory then
            Search (Directory => Full_Name (E),
                    Pattern   => "*",
                    Filter    => Filter,
                    Process   => Recursive_Add'Access);
         else
            Path_Vector.Append (UBS.To_Unbounded_String (Full_Name (E)));
         end if;
      end Recursive_Add;
   begin
      -- For System repositories, we want to make sure the AURA spec matches
      if Repo.Format = System then
         declare
            use Ada.Streams.Stream_IO;
            use Stream_Hashing;
            
            AURA_Unit: constant Registrar.Library_Units.Library_Unit
              := Registrar.Queries.Lookup_Unit 
                (Unit_Names.Set_Name ("aura"));
            
            Repo_AURA_Spec: File_Type;
            Repo_AURA_Hash: Hash_Type;
         begin
            Open (File => Repo_AURA_Spec,
                  Mode => In_File,
                  Name => UBS.To_String (Repo.Cache_Path) & "/aura.ads");
            
            Repo_AURA_Hash := Digest_Stream (Stream (Repo_AURA_Spec));
            
            Ada.Assertions.Assert 
              (Check   => Repo_AURA_Hash = AURA_Unit.Spec_File.Hash,
               Message => "System repository's AURA package does not match "
                 &        "the local AURA package");
         end;
      end if;
      
      -- Start with the location of the repository, which shall be a directory
      Search (Directory => UBS.To_String (Repo.Location),
              Pattern   => "*",
              Filter    => Filter,
              Process   => Recursive_Add'Access);
      
      Ada.Assertions.Assert (Check   => Path_Vector.Length > 0,
                             Message => "Location path is invalid");
      
      -- Set up the group tracker and dispatch
      Group_Tracker.Set_Total_Items (Natural (Path_Vector.Length));
      
      -- Dispatch
      for Path of Path_Vector loop
         New_Order.Path := Path;
         Workers.Enqueue_Order (New_Order);
      end loop;
      
   end Dispatch;
   
end Validate_Local_Or_System;
