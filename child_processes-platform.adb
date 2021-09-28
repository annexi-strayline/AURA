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

-- POSIX

with Interfaces.C.Strings;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

package body Child_Processes.Platform is
   
   pragma Assert (Stream_Element'Size = 8);
   -- Assumption for read/write system calls which take a void *, and
   -- a "nbytes" argument
   
   pragma Linker_Options ("-lc");
   pragma Linker_Options ("child_processes-platform-binding.o");
   
   use Interfaces.C;
   
   function fork_and_exec (path: in     char_array;
                           args: in     Interfaces.C.Strings.chars_ptr_array;
                           wdir: in     char_array;
                           
                           pid :    out Process_ID;
                           stdin:   out Stream_Handle;
                           stdout:  out Stream_Handle;
                           stderr:  out Stream_Handle)
                          return int
   with 
     Import        => True, 
     Convention    => C, 
     External_Name => "__chldproc_platform_fork_and_exec";
   -- 1. Creates new pipes for stdio
   -- 2. Forks
   -- 3. Parent sets read pipes to non-blocking,
   --    and sets the relevent out parameters.
   --    -1 is returned if the fork itself fails
   -- 4. Child attaches pipes to stdio
   -- 5. Child sets workding directory to wdir, then execv's path
   
   
   function wait_pid_terminate (pid: Process_ID) return int with
     Import        => True,
     Convention    => C,
     External_Name => "__chldproc_platform_wait_pid_terminate";
   -- Waits for pid to terminate, and returns the exit_value.
   -- If the wait system call fails, SIGTERM is sent to pid, and exit_value is
   -- set to -1
   
   procedure sigterm_kill (pid: in Process_ID) with
     Import        => True,
     Convention    => C,
     External_Name => "__chldproc_platform_sigterm_kill";   
   
   procedure sigkill_kill (pid: in Process_ID) with
     Import        => True,
     Convention    => C,
     External_Name => "__chldproc_platform_sigkill_kill";   
   
   function wait_read (fd: in Stream_Handle; wait: access timeval) 
                      return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "__chldproc_platform_wait_read";
   
   function wait_write (fd: in Stream_Handle; wait: access timeval)
                       return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "__chldproc_platform_wait_write";
   -- Returns 1 iff fd is available for reading resp. writing
   
   -- libc direct calls
   function read (fd    : Stream_Handle; 
                  buf   : not null access Stream_Element;
                  nbytes: size_t)
                 return ssize_t
   with
     Import        => True,
     Convention    => C,
     External_Name => "read";
   
   function write (fd    : Stream_Handle; 
                   buf   : not null access constant Stream_Element;
                   nbytes: size_t)
                  return ssize_t
   with
     Import        => True,
     Convention    => C,
     External_Name => "write";
   
   function close (fd: Stream_Handle) return int with
     Import        => True,
     Convention    => C,
     External_Name => "close";
   
   --
   -- Timeval Coversion
   --
   
   ----------------
   -- To_Timeval --
   ----------------
   
   function To_Timeval (T: Duration) return timeval is
      Microsecond: constant := 0.000001;
   begin
      return tv: timeval do
         tv.tv_sec  := time_t (Float'Floor(Float(T)));
         tv.tv_usec := suseconds_t ((T - Duration(tv.tv_sec)) / Microsecond);
      end return;
   end To_Timeval;
   
   
   --
   -- Argument Processing
   --
   
   package C_String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Interfaces.C.Strings.chars_ptr,
      "="          => Interfaces.C.Strings."=");
   
   
   function Parse_Arguments (Path     : in String;
                             Arguments: in String)
                            return C_String_Vectors.Vector;
   -- Parses Path to derrive the "command name", by selecting a slice from the
   -- end of path up to the first '/'. This "command name" is then assigned to
   -- a newloy allocated  chars_ptr (char *), which is then added to the first
   -- index of a new vector.
   --
   -- Arguments are then parsed with each argument allocated a chars_ptr and
   -- appended to the vector.
   --
   -- Finally a chars_ptr is appended with the Null_Ptr value.
   --
   -- The resulting Vector is thus a null-terminated vector of chars_ptr's,
   -- that can be copied into an chars_ptr_array, (char **), and passed into
   -- fork_and_exec
   
   
   procedure Free_Vector (V: in out C_String_Vectors.Vector);
   -- Frees each chars_ptr of the vector, replacing each element with
   -- a Null_Ptr
   
   ---------------------
   -- Parse_Arguments --
   ---------------------
   
   function Parse_Arguments (Path     : in String;
                             Arguments: in String)
                            return C_String_Vectors.Vector
   is
      use Interfaces.C.Strings;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      
      Arg_V: C_String_Vectors.Vector;
   begin
      -- Get the "command name"
      declare
         First: Natural;
         Command_Name: chars_ptr;
      begin
         First := Index (Source  => Path,
                         Pattern => "/",
                         From    => Path'Last,
                         Going   => Backward);
         
         if First < Path'First then
            First := Path'First;
         else
            First := First + 1;
         end if;
         
         Command_Name := New_String (Path (First .. Path'Last));
         
         Arg_V.Append (Command_Name);
      end;
      
      -- Parse the arguments
      declare
         First: Natural := Arguments'First;
         Last : Natural := First - 1;
         
         Single_Argument: chars_ptr;
      begin
         while Last < Arguments'Last loop
            Last := Last + 1;
            
            First := Index_Non_Blank (Source => Arguments,
                                      From   => Last);
            
            Last := Index (Source  => Arguments,
                           Pattern => " ",
                           From    => First);
            
            if Last < Arguments'First then
               Last := Arguments'Last;
            else
               Last := Last - 1;
            end if;
            
            Single_Argument := New_String (Arguments (First .. Last));
            Arg_V.Append (Single_Argument);
         end loop;
      end;
      
      Arg_V.Append (Null_Ptr);
      
      return Arg_V;
   end Parse_Arguments;
   
   -----------------
   -- Free_Vector --
   -----------------
   
   procedure Free_Vector (V: in out C_String_Vectors.Vector) is
      use C_String_Vectors;
   begin
      for E of V loop
         Interfaces.C.Strings.Free (E);
      end loop;
   end Free_Vector;
   
   --------------------
   -- Create_Process --
   --------------------
   
   procedure Create_Process (Image_Path       : in     String;
                             Arguments        : in     String;
                             Working_Directory: in     String;
                             
                             ID               :    out Process_ID;
                             Standard_Input   :    out Stream_Handle;
                             Standard_Output  :    out Stream_Handle;
                             Standard_Error   :    out Stream_Handle)
   is
      argv_Vector: C_String_Vectors.Vector
        := Parse_Arguments (Path => Image_Path, Arguments => Arguments);
      
      argv: Interfaces.C.Strings.chars_ptr_array 
        (size_t(argv_Vector.First_Index) .. size_t(argv_Vector.Last_Index));
      
      Fork_Result: int;
   begin
      for I in argv'Range loop
         argv(I) := argv_Vector(Natural(I));
      end loop;
      
      Fork_Result := fork_and_exec 
        (path   => To_C (Image_Path),
         args   => argv,
         wdir   => To_C (Working_Directory),
         
         pid    => ID,
         stdin  => Standard_Input,
         stdout => Standard_Output,
         stderr => Standard_Error);
      
      Free_Vector (argv_Vector);
      
      if Fork_Result /= 0 then
         raise Spawn_Failure with "Unable to fork new process";
      end if;
   end Create_Process;
   
   ----------------------
   -- Wait_Termination --
   ----------------------
   
   procedure Wait_Termination (ID       : in     Process_ID;
                               Exit_Code:    out Exit_Status)
   is
      Return_Code: int;
   begin
      Return_Code := wait_pid_terminate (ID);
      
      if Return_Code = 0 then
         Exit_Code := Success;
      else
         Exit_Code := Failure;
      end if;
   end Wait_Termination;
   
   ----------
   -- Kill --
   ----------
   
   procedure Kill (ID: in Process_ID) is
   begin
      sigterm_kill (ID);
   end Kill;
   
   ----------
   -- Nuke --
   ----------
   
   procedure Nuke (ID: in Process_ID) is
   begin
      sigkill_kill (ID);
   end Nuke;
   
   -----------------
   -- Read_Buffer --
   -----------------
   
   procedure Read_Buffer (Stream: in     Stream_Handle;
                          Buffer:    out Stream_Element_Array;
                          Last  :    out Stream_Element_Offset)
   is 
      Bytes_Read: ssize_t;
   begin
      Bytes_Read := read (fd     => Stream,
                          buf    => Buffer(Buffer'First)'Access,
                          nbytes => Buffer'Length);
      
      if Bytes_Read < 0 then
         Last := Buffer'First - 1;
      else
         Last := Buffer'First 
           + Stream_Element_Offset (Bytes_Read)
           - 1;
      end if;
      
      pragma Assert (Last <= Buffer'Last);
      
   end Read_Buffer;
   
   ------------------
   -- Write_Buffer --
   ------------------
   
   procedure Write_Buffer (Stream: in     Stream_Handle;
                           Buffer: in     Stream_Element_Array;
                           Last  :    out Stream_Element_Offset)
   is
      Bytes_Written: ssize_t;
   begin
      Bytes_Written := write (fd     => Stream,
                              buf    => Buffer(Buffer'First)'Access,
                              nbytes => Buffer'Length);
      
      if Bytes_Written < 0 then
         Last := Buffer'First - 1;
      else
         Last := Buffer'First 
           + Stream_Element_Offset (Bytes_Written)
           - 1;
      end if;
      
      pragma Assert (Last <= Buffer'Last);
      
   end Write_Buffer;
   
   -------------------
   -- Wait_Can_Read --
   -------------------
   
   procedure Wait_Can_Read  (Stream   : in     Stream_Handle; 
                             Timeout  : in     Duration;
                             Timed_Out:    out Boolean)
   is
      TO: aliased timeval := To_Timeval (Timeout);
      Result: int;
   begin
      Result := wait_read (fd => Stream,
                           wait => (if Timeout > 0.0 then 
                                       TO'Access
                                    else
                                       null));
      
      if Result > 0 then
         Timed_Out := False;
      else
         Timed_Out := True;
      end if;
   end Wait_Can_Read;
   
   --------------------
   -- Wait_Can_Write --
   --------------------
   
   procedure Wait_Can_Write  (Stream   : in     Stream_Handle; 
                              Timeout  : in     Duration;
                              Timed_Out:    out Boolean)
   is
      TO: aliased timeval := To_Timeval (Timeout);
      Result: int;
   begin
      Result := wait_write (fd => Stream,
                            wait => (if Timeout > 0.0 then 
                                        TO'Access
                                     else
                                        null));
      
      if Result > 0 then
         Timed_Out := False;
      else
         Timed_Out := True;
      end if;
   end Wait_Can_Write;
   
   ------------------
   -- Close_Stream --
   ------------------
   
   procedure Close_Stream (Handle: in Stream_Handle) is
      Discard: int;
   begin
      Discard := close (Handle);
   end Close_Stream;
   
end Child_Processes.Platform;
