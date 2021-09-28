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

with Ada.Directories;
with Ada.IO_Exceptions;

package body Registrar.Source_Files is
   
   package DIR renames Ada.Directories;
   package SIO renames Ada.Streams.Stream_IO;
   package UBS renames Ada.Strings.Unbounded;
   
   --
   -- Source_File_Access
   --
   
   ----------
   -- Read --
   ----------
   
   procedure Read  (Stream: not null access Ada.Streams.Root_Stream_Type'Class;
                    Item  : out Source_File_Access)
   is begin
      Item := null;
   end;
   
   --
   -- Source_File
   --
   
   ---------------
   -- Full_Name --
   ---------------
   
   function Full_Name (Source: Source_File) return String is
     (UBS.To_String (Source.Full_Name));
   
   
   function Hash (Source: Source_File) return Stream_Hashing.Hash_Type is
     (Source.Current_Hash);
   
   ------------------
   -- Compute_Hash --
   ------------------
   
   procedure Compute_Hash (File: in out Source_File) is
      use SIO;
   begin
      pragma Assert (Is_Open (File.File_Actual));
      Reset (File => File.File_Actual, Mode => In_File);
      File.Current_Hash 
        := Stream_Hashing.Digest_Stream (Stream (File.File_Actual));
   end Compute_Hash;
   
   --
   -- Source_Stream
   --
   
   --------------------------
   -- Checkout_Read_Stream --
   --------------------------
   
   function Do_Checkout (Source : not null Source_File_Access;
                         Mode   : SIO.File_Mode;
                         Rewrite: Boolean)
                        return Source_Stream
   is
      Locked_Out: Boolean := False;
      
      use type Ada.Streams.Stream_IO.Stream_Access;
   begin
      Source.Stream_Checkout.Lock;
      Locked_Out := True;
      
      if not SIO.Is_Open (Source.File_Actual) then
         if not Rewrite then
            -- This will be the case most of the time
            SIO.Open (File => Source.File_Actual,
                      Mode => Mode,
                      Name => Full_Name (Source.all));
         else
            declare
               Name: constant String := Full_Name (Source.all);
            begin
               DIR.Delete_File (Name);
               SIO.Create (File => Source.File_Actual,
                           Mode => Mode,
                           Name => Name);
            end;
         end if;
         
      else
         if not Rewrite then
            -- When such files are initially allocated, they will be
            -- in the open state, trying to re-open them will raise
            -- an exception
            SIO.Reset (File => Source.File_Actual,
                       Mode => Mode);
         else
            SIO.Delete (Source.File_Actual);
            SIO.Create (File => Source.File_Actual,
                        Mode => Mode,
                        Name => Full_Name (Source.all));
         end if;
      end if;
      
      return S: Source_Stream do
         S.File          := Source;
         S.Checkout.File := Source;
         S.Stream_Actual := SIO.Stream (Source.File_Actual);
      end return;
      
   exception
      when others =>
         if Locked_Out then
            Source.Stream_Checkout.Release;
         end if;

         raise;
   end Do_Checkout;
   
   ----------------------------------------------------------------------------
   function Checkout_Read_Stream (Source: not null Source_File_Access)
                                      return Source_Stream
     is (Do_Checkout (Source  => Source,
                      Mode    => SIO.In_File,
                      Rewrite => False));
   
   ---------------------------
   -- Checkout_Write_Stream --
   ---------------------------
   
   function Checkout_Write_Stream (Source : not null Source_File_Access;
                                  Rewrite: in       Boolean := False)
                                       return Source_Stream
     is (Do_Checkout (Source  => Source,
                      Mode    => SIO.Append_File,
                      Rewrite => Rewrite));
   
   -----------------
   -- End_Of_File --
   -----------------
   
   function End_Of_File (Stream: in Source_Stream) return Boolean is
     (SIO.End_Of_File (Stream.File.File_Actual));
   
   -----------------------
   -- Position_At_Start --
   -----------------------
   
   procedure Position_At_Start (Stream: in out Source_Stream) is
   begin
      SIO.Reset (Stream.File.File_Actual);
   end Position_At_Start;
   
   ---------------------
   -- Position_At_End --
   ---------------------
   
   procedure Position_At_End (Stream: in out Source_Stream) is
      use SIO;
      
      File_Size: Count :=  Size (Stream.File.File_Actual);
   begin
      if File_Size > 0 then
         Set_Index (File => Stream.File.File_Actual,
                    To   => Positive_Count (File_Size));
      end if;
   end Position_At_End;
   
   ----------
   -- Read --
   ----------
   
   overriding
   procedure Read (Stream: in out Source_Stream;
                   Item  :    out Ada.Streams.Stream_Element_Array;
                   Last  :    out Ada.Streams.Stream_Element_Offset)
   is begin
      Stream.Stream_Actual.Read (Item => Item,
                                 Last => Last);
   end Read;
   
   -----------
   -- Write --
   -----------
   
   overriding
   procedure Write (Stream: in out Source_Stream;
                    Item  : in     Ada.Streams.Stream_Element_Array)
   is begin
      Stream.Stream_Actual.Write (Item);
   end Write;
   
   --
   -- Stream_Checkout_Lock
   --
   
   protected body Stream_Checkout_Lock is
      
      ----------
      -- Lock --
      ----------
      
      entry Lock when not Lock_Actual is
      begin
         Lock_Actual := True;
      end Lock;
      
      -------------
      -- Release --
      -------------
      
      procedure Release is
      begin
         Lock_Actual := False;
      end Release;
      
      ------------
      -- Locked --
      ------------
      
      function Locked return Boolean is (Lock_Actual);
   
   end Stream_Checkout_Lock;
   
   --
   -- Stream_Checkout_Token
   --
   
   --------------
   -- Finalize --
   --------------
   
   overriding
   procedure Finalize (Token: in out Stream_Checkout_Token) is
      use type SIO.File_Mode;
   begin
      if Token.File /= null then
         pragma Assert (SIO.Is_Open (Token.File.File_Actual));
         
         if SIO.Mode (Token.File.File_Actual) /= SIO.In_File then
            -- Recompute hash after modification
            Token.File.Compute_Hash;
         end if;
         
         SIO.Close (Token.File.File_Actual);
         Token.File.Stream_Checkout.Release;
      end if;
   end Finalize;
   
end Registrar.Source_Files;
