------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
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

package body Registrar.Protected_Map is
   
   ----------------------
   -- Protected_Master --
   ----------------------
   
   protected Protected_Master is
      -------------
      -- Queries --
      -------------
      
      function Contains_Element (Key: Key_Type) return Boolean;
      
      function Extract_Element (Key: Key_Type) return Element_Type;
      
      -------------------
      -- Modifications --
      -------------------
      
      procedure Insert (Key     : in     Key_Type;
                        New_Item: in     Element_Type;
                        Position:    out Maps.Cursor;
                        Inserted:    out Boolean);
      
      procedure Modify
        (Key    : in Key_Type;
         Process: not null access procedure (Item: in out Element_Type));
      
      procedure Insert_Or_Modify 
        (Key            : in Key_Type;
         New_Item       : in Element_Type;
         Process_Existing: not null access procedure 
           (Item: in out Element_Type));
      
   private
      Master: aliased Maps.Map;
   end Protected_Master;
   
   ----------------------------------------------------------------------------
   
   protected body Protected_Master is
      
      ----------------------
      -- Contains_Element --
      ----------------------
      
      function Contains_Element (Key: Key_Type) return Boolean is
        (Master.Contains (Key));
      
      ---------------------
      -- Extract_Element --
      ---------------------
      
      function Extract_Element (Key: Key_Type) return Element_Type is
      begin
         return Master(Master.Find (Key));
      end Extract_Element;
      
      ------------
      -- Insert --
      ------------
      
      procedure Insert (Key     : in     Key_Type;
                        New_Item: in     Element_Type;
                        Position:    out Maps.Cursor;
                        Inserted:    out Boolean) 
      is begin
         Master.Insert (Key      => Key,
                        New_Item => New_Item,
                        Position => Position,
                        Inserted => Inserted);
      end Insert;
      
      ------------
      -- Modify --
      ------------
      
      procedure Modify
        (Key    : in Key_Type;
         Process: not null access procedure (Item: in out Element_Type))
      is 
         I: constant Maps.Cursor := Master.Find (Key);
         
         procedure Process_Without_Key (Key    : in Key_Type;
                                        Element: in out Element_Type)
         is begin
            Process (Element);
         end Process_Without_Key;
      begin
         Master.Update_Element (Position => I,
                                Process  => Process_Without_Key'Access);
      end Modify;
   
      ----------------------
      -- Insert_Or_Modify --
      ----------------------
      
      procedure Insert_Or_Modify 
        (Key            : in Key_Type;
         New_Item       : in Element_Type;
         Process_Existing: not null access procedure 
           (Item: in out Element_Type))
      is 
         Position: Maps.Cursor;
         Inserted: Boolean;
      begin
         
         Insert (Key      => Key,
                 New_Item => New_Item,
                 Position => Position,
                 Inserted => Inserted);
         
         if not Inserted then
            Process_Existing (Master (Position));
         end if;
         
      end Insert_Or_Modify;
   
   end Protected_Master;
   
   ----------------------
   -- Contains_Element --
   ----------------------
   
   function Contains_Element (Key: Key_Type) return Boolean
     is (Protected_Master.Contains_Element (Key));
   
   ---------------------
   -- Extract_Element --
   ---------------------
   
   function Extract_Element (Key: Key_Type) return Element_Type
     is (Protected_Master.Extract_Element (Key));
   
   ------------
   -- Insert --
   ------------
   
   procedure Insert (Key     : in     Key_Type; 
                     New_Item: in     Element_Type;
                     Inserted:    out Boolean)

   is 
      Dont_Care: Maps.Cursor;
   begin
      Protected_Master.Insert (Key      => Key,
                               New_Item => New_Item,
                               Position => Dont_Care,
                               Inserted => Inserted);
   end Insert;
   
   ------------
   -- Modify --
   ------------
   
   procedure Modify 
     (Key    : in Key_Type;
      Process: not null access procedure (Item: in out Element_Type))
   is begin
      Protected_Master.Modify (Key     => Key,
                               Process => Process);
   end Modify;
   
   ----------------------
   -- Insert_Or_Modify --
   ----------------------
   
   procedure Insert_Or_Modify 
     (Key             : in Key_Type;
      New_Item        : in Element_Type;
      Process_Existing: not null access procedure (Item: in out Element_Type))
   is
   begin
      Protected_Master.Insert_Or_Modify (Key              => Key,
                                         New_Item         => New_Item,
                                         Process_Existing => Process_Existing);
   end Insert_Or_Modify;
   
end Registrar.Protected_Map;
