------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                                 Core                                     --
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

-- Task-safe set container optimized for parallel access

with Ada.Containers.Hashed_Sets;

generic
   type Element_Type is private;
   with package Sets is new Ada.Containers.Hashed_Sets 
     (Element_Type => Element_Type, others => <>);
package Registrar.Protected_Set is
   
   -------------
   -- Queries --
   -------------
   
   function Contains_Element (Match: Element_Type) return Boolean;
   
   -- True if the Set contains an Element that matches Match
   
   function Extract_Element (Match: Element_Type) return Element_Type;
   
   -- Returns the element from the Set that matches Match
   --
   -- Constraint_Error is raised if no match is found.
   
   function Extract_Subset 
     (Filter: not null access function (Element: Element_Type)
                                       return Boolean)
     return Sets.Set;
   
   -- Returns a set containing all elements for which Filter returned
   -- True for that element. This is done through iteration. 
   
   function Extract_Subset (Match_Set: Sets.Set) return Sets.Set;
   -- Returns a subset containing all elements that exist in both the
   -- master set, and Match_Set (Intersection)
   
   function Extract_Set return Sets.Set;
   -- Return the entire (master) set
   
   function Is_Subset (Query_Set: Sets.Set) return Boolean;
   -- Returns True if Query_Set is a subset of the master set.
   
   -------------------
   -- Modifications --
   -------------------
   
   procedure Insert (New_Item: in     Element_Type;
                     Inserted:    out Boolean);
   
   -- Attempts to insert New_Item into the Set. 
   --
   -- If New_Item already exists in the set, Inserted is False, and
   -- the Set is not modified.
   
   procedure Include (New_Item: in Element_Type);
   -- If an equivalent item already exists in the set, it is replaced by
   -- New_Item
   
   procedure Include_Subset (New_Items: in Sets.Set);
   -- Performs Difference on Master with New_Items, and then a Union.
   
   procedure Replace (New_Item: in Element_Type);
   -- Replaces an existing item in the set. If that item does not already
   -- exist, Constraint_Error is propagated.
   
   procedure Union (Input_Set: in Sets.Set);
   
   -- Inserts all item of Input_Set that are not currently in the Set
   
   procedure Modify 
     (Match  : in Element_Type;
      Process: not null access procedure (Item: in out Element_Type));
   
   -- Attempts to look up the element from Set that matches Match, and
   -- then passes that Item to Process for modification
   --
   -- Constraint_Error is raised if Match does not select an element in
   -- the Set.
   
   procedure Delete_Element (Match: in Element_Type);
   -- Deletes the element identified by Match from the set.
   
end Registrar.Protected_Set;
