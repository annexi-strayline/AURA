------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020-2021, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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
with Ada.Streams.Stream_IO;

with Registrar.Registration;
with Stream_Hashing.Collective;

separate (Build)

package body Hash_Compilation_Orders is
   
   use Registrar.Library_Units;
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Hash_Compilation_Order) return String is
     ("[Hash_Compilation_Order] (Build)" & New_Line
        & " Target unit: " & Order.Target.Name.To_UTF8_String);
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Hash_Compilation_Order) is
      use Stream_Hashing.Collective;
      
      Collection: Hash_Collections.Set;
      
      procedure Include_Hash (Path: in String) is
         use Ada.Streams.Stream_IO;
         File: File_Type;
      begin
         Open (File => File,
               Mode => In_File,
               Name => Path);
         Collection.Include (Stream_Hashing.Digest_Stream (Stream (File)));
         Close (File);
      end Include_Hash;
      
      Object_Path: constant String := Object_File_Name (Order.Target);
      ALI_Path   : constant String := ALI_File_Name (Order.Target);
      
      Have_Object: constant Boolean := Ada.Directories.Exists (Object_Path);
      Have_ALI   : constant Boolean := Ada.Directories.Exists (ALI_Path);
   begin
      pragma Assert (Order.Target.Kind not in Unknown | Subunit
                       and then Order.Target.State in Available | Compiled);
      
      -- Don't waste time if we have no object or ALI!
      if (not Have_Object) and (not Have_ALI) then return; end if;
      
      -- For units that are not External_Units, we expect to have an ".ali".
      -- file as well as .o file. It is all or nothing. If we don't find both,
      -- then we want to delete the ones we do have.
      --
      -- For subsystems checked-out from "System" repository, Object_Path will
      -- return a path to the library shared object.
      --
      -- Otherwise, we expect that an External Unit will not have an ALI file
      
      if Order.Target.Kind = External_Unit then
         Ada.Assertions.Assert 
           (Check => not Have_ALI,
            Message => "External_Units should not have .ali files.");
         
      elsif Have_Object xor Have_ALI then
         if Have_Object then
            Ada.Directories.Delete_File (Object_Path);
         else
            Ada.Directories.Delete_File (ALI_Path);
         end if;
         
         return;
         
      end if;
      
      if Have_Object then Include_Hash (Object_Path); end if;
      if Have_ALI    then Include_Hash (ALI_Path);    end if;
      
      Compute_Collective_Hash 
        (Collection      => Collection,
         Collective_Hash => Order.Target.Compilation_Hash);
      
      Order.Target.State := Compiled; 
      -- If we got this far, it definately means the unit was compiled, so
      -- we can promote that unit to being such
      
      Registrar.Registration.Update_Library_Unit (Order.Target);
   end Execute;

end Hash_Compilation_Orders;
