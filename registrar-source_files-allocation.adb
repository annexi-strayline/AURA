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
with Ada.Unchecked_Deallocation;

package body Registrar.Source_Files.Allocation is
   
   package SIO renames Ada.Streams.Stream_IO;
   
   generic
      with procedure Action (File: in out SIO.File_Type;
                             Mode: in     SIO.File_Mode;
                             Name: in     String;
                             Form: in     String := "");
   
   procedure Open_Or_Create (Source   : in out Source_File;
                             Full_Name: in     String);
   
   procedure Open_Or_Create (Source   : in out Source_File;
                             Full_Name: in     String)
   is
      use Ada.Strings.Unbounded;
   begin
      -- Note that we don't need to worry about a checked-out stream, since
      -- that can only be done if the file is already open, and attempting
      -- open on an already open file will just raise an exception, and it
      -- won't bother anyone.
      
      -- We attempt to open the file first, since if it fails, we won't
      -- set the Full_Name property
      
      Action (File => Source.File_Actual,
              Mode => SIO.In_File,
              Name => Full_Name);
      
      Set_Unbounded_String (Target => Source.Full_Name,
                            Source => Full_Name);
   end Open_Or_Create;
   
   ----------
   -- Open --
   ----------
   
   function Open (Full_Name: String) return not null Source_File_Access
   is
      procedure Do_Open is new Open_Or_Create (Action => SIO.Open);
   begin
      return Source: not null Source_File_Access := new Source_File do
         Do_Open (Source    => Source.all, 
                  Full_Name => Full_Name);
         Source.Compute_Hash;
      end return;
   end Open;
   
   ------------
   -- Create --
   ------------
   
   function Create (Full_Name: String)
                   return not null Source_File_Access
   is
      procedure Do_Create is new Open_Or_Create (Action => SIO.Create);
   begin
      return Source: not null Source_File_Access := new Source_File do
         Do_Create (Source => Source.all, Full_Name => Full_Name);
      end return;
   end Create;
   
   -------------
   -- Discard --
   -------------
   
   procedure Discard (File    : in out Source_File_Access;
                      Checkout: in out Source_Stream'Class)
   is 
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Source_File,
         Name   => Source_File_Access);
   begin
      pragma Assert (Checkout.File = File);
      
      -- First nullify all links in the checked-out stream
      Checkout.Checkout.File := null;
      Checkout.File          := null;
      Checkout.Stream_Actual := null; -- collateral
      
      -- Close the file and free it
      if SIO.Is_Open (File.File_Actual) then
         SIO.Close (File.File_Actual);
      end if;
      
      Free (File);
      
   end Discard;
   
   
end Registrar.Source_Files.Allocation;
