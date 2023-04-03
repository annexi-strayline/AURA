------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                         Reference Implementation                         --
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Configuration;
with User_Notices;
with Host_Operations;
with Registrar.Queries;
with Registrar.Source_Files;
with Registrar.Registration.Unchecked_Deregister_Unit;

separate (Checkout)

package body Checkout_Orders is
   
   package UBS renames Ada.Strings.Unbounded;
   use type Repositories.Repository_Cache_State;
   use type Registrar.Subsystems.Subsystem_State;
   
   ------------------------
   -- Recursive_Copy_Dir --
   ------------------------
   
   procedure Recursive_Copy_Dir (Source_Dir, Target_Dir: String);
   -- Recursively copies Source_Dir into a new directory within Target_Dir
   -- I.e. Source => /x/y/z  Target => /a/b recursively copies the content
   -- of /x/y/z into a new directory a/b/z
   
   procedure Recursive_Copy_Dir (Source_Dir, Target_Dir: String) is
      use Ada.Directories;
      
      New_Target: constant String
        := Compose (Containing_Directory => Target_Dir,
                    Name                 => Simple_Name (Source_Dir));
      
      Search: Search_Type;
      File  : Directory_Entry_Type;
   begin
      Create_Path (New_Target);
      Start_Search (Search    => Search,
                    Directory => Source_Dir,
                    Pattern   => "*",
                    Filter    => (Special_File => False, others => True));
      
      while More_Entries (Search) loop
         Get_Next_Entry (Search          => Search,
                         Directory_Entry => File);
         
         -- Don't copy anything starting with "."
         if Ada.Strings.Fixed.Head (Source => Simple_Name (File),
                                    Count  => 1)
           /= "."
         then
            
            case Kind (File) is
               when Directory =>
                  Recursive_Copy_Dir (Source_Dir => Full_Name (File),
                                      Target_Dir => New_Target);
                  
               when Ordinary_File =>
                  Copy_File (Source_Name => Full_Name (File),
                             Target_Name => Compose 
                               (Containing_Directory => New_Target,
                                Name                 => Simple_Name (File)));
                  
               when Special_File =>
                  -- Precluded by the filter
                  raise Program_Error;
            end case;
         end if;
      end loop;
      
   end Recursive_Copy_Dir;
   
   ---------------------------
   -- Link_From_System_Repo --
   ---------------------------
   
   procedure Link_From_System_Repo 
     (Repo_Path: in String;
      Subsys   : in Registrar.Subsystems.Subsystem)
   is
      use Ada.Directories;
      
      Config_Unit_Name: constant Unit_Names.Unit_Name
        := Configuration.Config_Unit_Name (Subsys);
      
      Notice: User_Notices.Notice_Lines;
      function To_UBS (Source: in String) return UBS.Unbounded_String
        renames UBS.To_Unbounded_String;
      
   begin
      if Registrar.Queries.Unit_Entered (Config_Unit_Name) then
         -- System repository checkouts are a bit unusual. System repos are
         -- special repos that have two subdirectories - include and lib.
         -- Inside include are subdirectories for each subsystem. Each
         -- subsystem subdirectory contains a single directory with all the
         -- sources of the original AURA subsystem, including the contents of
         -- configured codepaths, as well as all ali files (for GNAT)
         --
         -- The lib subdirectory contains the compiled shared library files for
         -- each subsystem
         --
         -- Because "System" repositories are pre-compiled, they must also be
         -- already configured. Therefore the configuration unit is included in
         -- the include/subsystem subdirectory.
         --
         -- Before we link in the subsystem, we need to ensure that no
         -- configuration unit for that subsystem has been entered. If it has,
         -- we back-up the existing unit and scrub it from the registrar
         
         Notice.Append 
           (To_UBS ("Subsystem """  & Subsys.Name.To_UTF8_String & '"'));
         
         Notice.Append
           (To_UBS ("is to be checked-out from Repository" 
                      & Repositories.Repository_Index'Image
                        (Subsys.Source_Repository)
                      & ", which is a ""System"" repo."));
         
         Notice.Append
           (To_UBS ("An existing Configuration Unit was found for this " 
                      & "subsystem."));
         
         Notice.Append
           (To_UBS ("Local Configuration Units are not allowed for "
                      & "subsystems."));
         
         Notice.Append
           (To_UBS ("A backup will be created."));
         
         User_Notices.Post_Notice (Notice);
         
         declare
            use type Registrar.Source_Files.Source_File_Access;
            
            Config_Unit: Registrar.Library_Units.Library_Unit
              := Registrar.Queries.Lookup_Unit (Config_Unit_Name);
            
            Spec_Path: constant String := Config_Unit.Spec_File.Full_Name;
            Body_Path: constant String 
              := (if Config_Unit.Body_File /= null then
                     Config_Unit.Body_File.Full_Name
                  else
                     "");
         begin
            Registrar.Registration.Unchecked_Deregister_Unit (Config_Unit);
            -- This is is safe because config units are not allowed to have
            -- children or subunits, and also cannot depend on anything except
            -- the standard library. These things are enforced by the Registrar
            -- on entry.
            
            -- The linked subsystem, which will be entered at
            -- Complete_Registration, will contain the configuration unit.
            
            Ada.Directories.Rename (Old_Name => Spec_Path,
                                    New_Name => Spec_Path & ".bak");
            
            if Body_Path /= "" then
               Ada.Directories.Rename (Old_Name => Body_Path,
                                       New_Name => Body_Path & ".bak");
            end if;
            
         end;
         
      end if;
      
      -- Now we can link in the subdirectory
      Host_Operations.Symbolic_Link 
        (Target_Path => Repo_Path,
         Source_Path => Current_Directory & '/' & Subsys.Name.To_UTF8_String);
   end Link_From_System_Repo;
   
   
   -------------------------
   -- Checkout_From_Cache --
   -------------------------
   
   procedure Checkout_From_Cache 
     (Index : in     Repositories.Repository_Index;
      Target: in     Registrar.Subsystems.Subsystem;
      Done  :    out Boolean)
   is
      use Ada.Directories;
      use type Repositories.Repository_Format;
      
      Search: Search_Type;
      CO_Source: Directory_Entry_Type;
      Repo: constant Repositories.Repository
        := Repositories.Extract_Repository (Index);

   begin
      Start_Search 
        (Search    => Search,
         Directory => UBS.To_String (Repo.Cache_Path)
           & (if Repo.Format = Repositories.System then
                 "/include"
              else
                 ""),
         Pattern   => Target.Name.To_UTF8_String,
         Filter    => (Directory => True, others => False));
      
      if not More_Entries (Search) then
         -- Subsystem not found in this repo..
         Done := False;
         return;
      end if;
      
      Get_Next_Entry (Search          => Search,
                      Directory_Entry => CO_Source);
      
      case Repo.Format is
         when Repositories.System =>
            Link_From_System_Repo (Repo_Path => Full_NAme (CO_Source),
                                   Subsys    => Target);
            
         when Repositories.Local | Repositories.Git =>
            Recursive_Copy_Dir (Source_Dir => Full_Name (CO_Source),
                                Target_Dir => Current_Directory);
      end case;
            
      Done := True;
   end Checkout_From_Cache;
   
   ---------------------------
   -- Complete_Registration --
   ---------------------------
   
   procedure Complete_Registration
     (Target    : in out Registrar.Subsystems.Subsystem;
      Imperative: in     Boolean := True) 
   is
      use Ada.Directories;
      use type Repositories.Repository_Format;
      
      Search: Search_Type;
      Dir   : Directory_Entry_Type;
      
      Repo: constant Repositories.Repository
        := Repositories.Extract_Repository (Target.Source_Repository);
   begin
      -- We are expecting the subsystem source subdirectory to be available
      
      Start_Search (Search    => Search,
                    Directory => Current_Directory,
                    Pattern   => Target.Name.To_UTF8_String,
                    Filter    => (Directory => True, others => False));
      
      if More_Entries (Search) then
         -- Directory exists. This means its already aquired
         
         Get_Next_Entry (Search => Search, Directory_Entry => Dir);
         
         -- Update the subsystem status
         case Repo.Format is
            when Repositories.System =>
               Target.State := Registrar.Subsystems.Available;
               -- System repositories are "preconfigured" in a sense.
               
            when others =>
               Target.State := Registrar.Subsystems.Aquired;
               
         end case;
               
         Registrar.Registration.Update_Subsystem (Target);
         
         -- Dispatch contents to the registrar
         pragma Assert (Target.AURA);
         Registrar.Registration.Enter_Directory
           (Directory      => Dir,
            AURA_Subsystem => Target);
         
      else
         Assert (Check => not Imperative,
                 Message => "Unable to register checked-out subsystem");
         
      end if;
   end Complete_Registration;
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Checkout_Order) return String
     is (    "[Checkout_Order]" & New_Line
           & " Subsystem: " & Order.Target.Name.To_UTF8_String);
   
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Checkout_Order) is
      package Queries renames Registrar.Queries;
      use type Repositories.Repository_Index;
      use type Repositories.Repository_Count;
      
      procedure No_Repo_Abort (Reason: in String) is
         -- There are a few cases where a subsystem cannot be aquired for
         -- a common reason, such as the checkout repository not being
         -- registered, the registered repository not having the subsystem,
         -- or (in the case of Root Repository sourced subsystems) not existing
         -- in any of the configured repository.
         --
         -- In these cases we want to register a failure, and also report
         -- the reason in a user-friendly message and pass that up to the
         -- CLI in a way that allows it to handle the error more gracefully
         -- than aborting with work reports. Particularily this allows the
         -- CLI to build the dependency maps and inform the user which
         -- units/subsystems depend on the missing subsystems.
         --
         -- In order to facilitate this orderly failure, this subprogram
         -- takes-over the tracker, registering a failure directly and
         -- removing the responsibility of the executing worker of doing
         -- so.
         --
         -- Obviously this should only be called immediately before the
         -- order is completed.
      begin
         UBS.Set_Unbounded_String (Target => Order.Target.Aquisition_Failure,
                                   Source => Reason);
         Order.Target.State := Registrar.Subsystems.Unavailable;
         Registrar.Registration.Update_Subsystem (Order.Target);
         Order.Tracker.Increment_Failed_Items;
         Order.Tracker := null;
      end No_Repo_Abort;
      
   begin
      -- Look for the checkout unit and then submit
      declare
         use Unit_Names;
         
         Checkout_Unit_Name: constant Unit_Name
           := Set_Name 
             ("AURA." & Wide_Wide_String'(Order.Target.Name.To_String) 
                & ".Checkout"); 
      begin
         if Queries.Unit_Entered (Checkout_Unit_Name) then
            -- Pull it down and parse it
            
            declare
               use Registrar.Library_Units;
               
               Checkout_Unit: constant Library_Unit
                 := Queries.Lookup_Unit (Checkout_Unit_Name);
            begin

               Order.Target.Source_Repository
                 := Parse_Checkout_Spec (Checkout_Unit);
            end;

         else
            -- No spec, use the default Root Repository for now, and
            -- write out
            Order.Target.Source_Repository := Repositories.Root_Repository;
            Write_Checkout_Spec (Order.Target);
            
            -- We now need to return for another cycle, where the new checkout
            -- that was just registered will be fully entered into the
            -- registrar.
            --
            -- It is important to do this, otherwise we will potentially re-
            -- write the spec and submit a duplicate Entry order for the same
            -- checkout unit, which will cause AURA to abort.
            return;
         end if;
      end;
      
      -- This means we need to aquire the subsystem source from a repository of
      -- some kind. Specification is now in-place
      
      -- Make sure the indicated repository actually exists
      
      if Order.Target.Source_Repository > Repositories.Total_Repositories then
         No_Repo_Abort ("Configured Repository" 
                          &        Repositories.Repository_Index'Image
                            (Order.Target.Source_Repository) 
                          &        " is not registered.");
         return;
      end if;
      
      
      -- If the code is already present, no need to check it out. Just enter it
      -- and promote the Subsystem to Aquired. This is the usual process of
      -- Complete_Checkout 
      
      Complete_Registration (Target     => Order.Target,
                             Imperative => False);
      
      -- If Complete_Registration saw the expected subdirectory for the
      -- subsystem in the root directory, Order.Target.State will be set to
      -- Aquired, and so we can leave now. Otherwise it indicates we need to
      -- checkout from a cache, if possible
      
      if Order.Target.State = Registrar.Subsystems.Aquired then
         return;
      end if;
      
      -- If we get here, this subsystem needs to be checked-out (eventually)
      
      if Order.Target.Source_Repository = Repositories.Root_Repository then
         -- We don't have a source directory, but the repository is set
         -- to the Root Repo. This indicates that we need to search the first
         -- repository to have the subsystem, since we can't actually check
         -- anything out from the Root Repo.
         
         -- If the root repository is the only repository, then we're clearly
         -- out of luck.
         
         if Repositories.Total_Repositories = Repositories.Root_Repository then
            -- We cant check-out from the root repo!
            No_Repo_Abort ("Subsystem does not exist in any configured "
                             & "repository.");
            return;
         end if;
         
         -- Scan for a repository that might have what we are looking
         -- for. If we find it, we can set to that repository and check
         -- out the code. If we don't find it, we will ask for all
         -- repositories to be cached, and retry in the next cycle.
         -- If all repositories are cached, the checkout attempt fails
         
         for I in 
           Repositories.Root_Repository + 1 .. Repositories.Total_Repositories
         loop
            if Repositories.Cache_State(I) = Repositories.Available then
               declare
                  Done: Boolean;
               begin
                  Checkout_From_Cache (Index  => I,
                                       Target => Order.Target,
                                       Done   => Done);
                  
                  if Done then
                     -- She's a keeper. Update the target's checkout
                     -- to be of this repo, and write out the spec
                     Order.Target.Source_Repository := I;
                     Write_Checkout_Spec (Order.Target);
                     Complete_Registration (Order.Target);
                     exit;
                     
                  elsif I = Repositories.Total_Repositories then
                     -- If we get here and I is at the last repository, it
                     -- means that all repositories are cached, but yet we
                     -- still couldn't find it
                     No_Repo_Abort ("Subsystem does not exist in any "
                                      & "configured repository.");
                     return;
                  end if;
               end;
            else
               Repositories.Request_Cache (I);
               -- This means we will request caching of every repo that is
               -- not currently available, if we can't find the subsystem
               -- in a currently cached repo
            end if;
         end loop;
         
      else
         -- Normal non Root Repo
         --
         -- If the repo is cached, we can do a checkout, otherwise, we will
         -- request a cache and re-try next cycle. If it is cached, it
         -- better be there!
         
         case Repositories.Cache_State (Order.Target.Source_Repository) is
            when Repositories.Available =>
               
               declare
                  Checkout_OK: Boolean;
               begin
                  Checkout_From_Cache 
                    (Index  => Order.Target.Source_Repository,
                     Target => Order.Target,
                     Done   => Checkout_OK);
                  
                  if not Checkout_OK then
                     No_Repo_Abort 
                       (    "Subsystem does not exist within the "
                          & "designated repository (Repository" 
                          & Repositories.Repository_Index'Image
                            (Order.Target.Source_Repository)
                          & ')');
                     return;
                  end if;
               end;
               
               Complete_Registration (Order.Target);
               
            when Repositories.Requested =>
               -- Already requested, nothing to do this round
               null;
               
            when Repositories.Standby =>
               -- Not cached. Try again in the next cycle
               Repositories.Request_Cache (Order.Target.Source_Repository);
         end case;
         
      end if;
      
   end Execute;
   
end Checkout_Orders;
