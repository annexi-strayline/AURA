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

with Ada.Containers.Hashed_Maps;

generic
   type Key_Type is private;
   type Element_Type is private;
   
   with package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type     => Key_Type,
      Element_Type => Element_Type,
      others       => <>);
   
package Registrar.Protected_Map is
   
   -------------
   -- Queries --
   -------------
   
   function Contains_Element (Key: Key_Type) return Boolean;
   
   -- Returns True if Key maps to an element.
   
   function Extract_Element (Key: Key_Type) return Element_Type;
   
   -- Returns a copy of the element mapped to Key
   --
   -- Constraint_Error is raised if Key does not map to an Element
   
   -------------------
   -- Modifications --
   -------------------
   
   procedure Insert (Key     : in     Key_Type; 
                     New_Item: in     Element_Type;
                     Inserted:    out Boolean);
   
   -- Attempts to insert New_Element at Key. 
   --
   -- If an Element already maps to Key, Inserted is False, and the Map is not
   -- modified.
   
   procedure Modify 
     (Key    : in Key_Type;
      Process: not null access procedure (Item: in out Element_Type));
   
   -- Attempts to look up the element that maps to Key, and then passes that 
   -- Item to Process for modification
   --
   -- Constraint_Error is raised if Key does not select an element in
   -- the Map.
   
   procedure Insert_Or_Modify 
     (Key             : in Key_Type;
      New_Item        : in Element_Type;
      Process_Existing: not null access procedure (Item: in out Element_Type));
   
   -- Attempts to insert New_Element at Key. If the item already exists,
   -- Modify_Existing is invoked with the existing item instead.
   
end Registrar.Protected_Map;
