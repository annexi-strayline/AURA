------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019-2020, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- The Source_Files package provides a single type for managing individual
-- source files. Once initialized, a source file is opened and left open.
--
-- Stream access is controlled, such that only one stream may be active
-- at any given time, since multiple stream access to file objects is not
-- generally considered workable

with Ada.Streams;
with Ada.Containers.Vectors;
private with Ada.Finalization;
private with Ada.Streams.Stream_IO;
private with Ada.Strings.Unbounded;

with Stream_Hashing;

package Registrar.Source_Files is
   
   -----------------
   -- Source_File --
   -----------------
   
   type Source_File (<>) is tagged limited private;
   
   -- Source_File objects are allocated from the Standard Storage Pool, and
   -- should have a life that is the same as the program (never deallocated)
   --
   -- The registrar has exclusive control over the allocation and deallocation
   -- of source files.
   --
   -- Any source files entered into an accessible registry will never be
   -- deallocated.
   --
   -- Unchecked_Deallocation must never be instantiated for Source_File_Access
   -- except in the child private package Allocation.
   
   
   type Source_File_Access is access Source_File;
   
   procedure Read  (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
                    Item  : out Source_File_Access);
   
   procedure Write (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
                    Item  : in Source_File_Access) 
     is null;
   
   for Source_File_Access'Stream_Size use 0;
   for Source_File_Access'Read        use Read;
   for Source_File_Access'Write       use Write;
   
   -- We don't write anything, and always pretend to read-in null. This is to
   -- facilitate rational 'Write and 'Read operations when streaming 
   -- Library_Units
   
   
   function  Full_Name (Source: Source_File) return String;
   -- Returns the "full name" of the file as defined by the Ada.Directories
   -- language-defined package, or a null string if the file is not open
   
   function  Hash (Source: Source_File) return Stream_Hashing.Hash_Type;
   -- The stream hash is computed on allocation of the file, and on
   -- the finalization of a Read_Write checkout. 
   --
   -- This value is not necessarily task-safe, but it's use is expected to
   -- be used during phases where source files are not being modified.
   --
   -- The Hash value is always updated before the checkout is released
   
   -- Source_File_Vectors --
   -------------------------
   package Source_File_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Source_File_Access);
   
   -------------------
   -- Source_Stream --
   -------------------
   
   type Source_Stream (<>) is limited new Ada.Streams.Root_Stream_Type 
     with private;
   
   -- Source_Streams give task-safe stream access to a given Source_File object
   --
   -- The Stream of a Source_File may only have a single Source_Stream checked-
   -- out at any given time.
   --
   -- Attempting a check-out on an already checked-out stream blocks until the
   -- already checked-out stream is released.
   --
   -- The underlying file is always opened (with the appropriate mode) when
   -- the stream is checked-out and closed when the check-out is released.
   --
   -- The effects of invoking Write on a Read_Only Stream should be expected to
   -- reflect an attempt to invoke Write on a Stream_IO File set to mode 
   -- In_File.
   
   not overriding
   function Checkout_Read_Stream (Source: not null Source_File_Access)
                                      return Source_Stream;
   
   not overriding
   function Checkout_Write_Stream (Source : not null Source_File_Access;
                                   Rewrite: in       Boolean := False)
                                       return Source_Stream;
   -- If Rewrite is True, the file is first deleted, and then created
   
   not overriding
   function End_Of_File (Stream: in Source_Stream) return Boolean;
   -- Equivilent to Ada.Streams.Stream_IO.End_Of_File on the underlying
   -- file
   
   not overriding
   procedure Position_At_Start (Stream: in out Source_Stream);
   -- Resets the Position of the file
   
   not overriding
   procedure Position_At_End (Stream: in out Source_Stream);
   -- Positions the index of the file to the end
   
   overriding
   procedure Read (Stream: in out Source_Stream;
                   Item  :    out Ada.Streams.Stream_Element_Array;
                   Last  :    out Ada.Streams.Stream_Element_Offset);
   
   overriding
   procedure Write (Stream: in out Source_Stream;
                    Item  : in     Ada.Streams.Stream_Element_Array);
   
private
   
   --------------------------
   -- Stream_Checkout_Lock --
   --------------------------
   
   -- A single lock shared by all Source_File objects. Stream_Checkout_Tokens
   -- are linked to Source_File, which contains the lock.
   
   protected type Stream_Checkout_Lock is
      entry      Lock;
      procedure  Release;
      
      function   Locked return Boolean;
   private
      Lock_Actual: Boolean := False;
   end Stream_Checkout_Lock;
   
   
   -----------------
   -- Source_File --
   -----------------
   
   type Source_File is tagged limited
      record
         File_Actual    : Ada.Streams.Stream_IO.File_Type;
         Full_Name      : Ada.Strings.Unbounded.Unbounded_String;     
         Current_Hash   : Stream_Hashing.Hash_Type;
         Stream_Checkout: Stream_Checkout_Lock;
      end record;
   
   not overriding
   procedure Compute_Hash (File: in out Source_File);
   -- Computes the hash of File, and sets the Current_Hash component to the
   -- resulting value. This procedure should be called either when a
   -- Source_File is allocated, or while it is still checked-out.
   --
   -- File is expected to be Open, is Reset to In_File before computing the
   -- hash.
   
   ---------------------------
   -- Stream_Checkout_Token --
   ---------------------------
   
   -- In lieu of being able to make a Root_Stream_Type also controlled, the
   -- Stream_Checkout_Token is as a component of Source_Stream which will
   -- be finalized with Source_Stream, and cause the token to be released
   
   type Stream_Checkout_Token is
     limited new Ada.Finalization.Limited_Controlled with
      record
         File: Source_File_Access := null;
      end record;
   
   overriding
   procedure Finalize (Token: in out Stream_Checkout_Token);
   
   -------------------
   -- Source_Stream --
   -------------------
   
   -- By linking the Source_Stream to a Source_File, and by also containing
   -- a Stream_Checkout_Token linked to the same Source_File, we can be sure
   -- that finalization of a Source_Stream object can release the lock
   
   type Source_Stream is limited 
     new Ada.Streams.Root_Stream_Type with
      record
         File         : Source_File_Access                  := null;
         Stream_Actual: Ada.Streams.Stream_IO.Stream_Access := null;
         Checkout     : Stream_Checkout_Token;
      end record;
   


   
end Registrar.Source_Files;
