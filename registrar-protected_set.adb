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

package body Registrar.Protected_Set is
   
   ----------------------
   -- Protected_Master --
   ----------------------
   
   protected Protected_Master is
      -------------
      -- Queries --
      -------------
      
      function Extract_Subset 
        (Filter: not null access function (Element: Element_Type)
                                          return Boolean)
        return Sets.Set;
      
      function Extract_Subset (Match_Set: Sets.Set) return Sets.Set;
      
      function Extract_Set return Sets.Set;
      
      function Contains_Element (Match: Element_Type) return Boolean;
      
      function Extract_Element (Match: Element_Type) return Element_Type;
      
      function Is_Subset (Query_Set: Sets.Set) return Boolean;
      
      -------------------
      -- Modifications --
      -------------------
      
      procedure Insert (New_Item: in     Element_Type;
                        Inserted:    out Boolean);
      
      procedure Include (New_Item: in Element_Type);
      
      procedure Include_Subset (New_Items: in Sets.Set); 
      
      procedure Replace (New_Item: in Element_Type);
      
      procedure Union (Input_Set: in Sets.Set);
      
      procedure Modify 
        (Match  : in Element_Type;
         Process: not null access procedure (Item: in out Element_Type));
      
      procedure Delete (Item: in Element_Type);
      
   private
      Master: aliased Sets.Set;
   end Protected_Master;
   
   ----------------------------------------------------------------------------
   
   protected body Protected_Master is
      
      ----------------------
      -- Contains_Element --
      ----------------------
      
      function Contains_Element (Match: Element_Type) return Boolean is
        (Master.Contains (Match));
      
      ---------------------
      -- Extract_Element --
      ---------------------
      
      function Extract_Element (Match: Element_Type) return Element_Type is
      begin
         return Master(Master.Find (Match));
      end Extract_Element;
      
      --------------------
      -- Extract_Subset --
      --------------------
      
      function Extract_Subset 
        (Filter: not null access function (Element: Element_Type)
                                          return Boolean)
        return Sets.Set 
      is begin
         return Selected_Set: Sets.Set do
            for E of Master loop
               if Filter (E) then
                  Selected_Set.Insert (E);
               end if;
            end loop;
         end return;
      end Extract_Subset;
      
      --------------------------------------------------
      function Extract_Subset (Match_Set: Sets.Set) return Sets.Set is
        (Master.Intersection (Match_Set));
      
      -----------------
      -- Extract_Set --
      -----------------
      
      function Extract_Set return Sets.Set is (Master);
      
      ---------------
      -- Is_Subset --
      ---------------
      
      function Is_Subset (Query_Set: Sets.Set) return Boolean is 
        (Query_Set.Is_Subset (Master));
      
      ------------
      -- Insert --
      ------------
      
      procedure Insert (New_Item: in     Element_Type;
                        Inserted:    out Boolean) 
      is 
         Dont_Care: Sets.Cursor;
      begin
         Master.Insert (New_Item => New_Item,
                        Position => Dont_Care,
                        Inserted => Inserted);
      end Insert;
      
      -------------
      -- Include --
      -------------
      
      procedure Include (New_Item: in Element_Type) is 
      begin
         Master.Include (New_Item);
      end Include;
      
      --------------------
      -- Include_Subset --
      --------------------
      
      procedure Include_Subset (New_Items: in Sets.Set) is
      begin
         Master.Difference (New_Items);
         Master.Union (New_Items);
      end Include_Subset;
      
      -------------
      -- Replace --
      -------------
      
      procedure Replace (New_Item: in Element_Type) is 
      begin
         Master.Replace (New_Item);
      end Replace;
      
      -----------
      -- Union --
      -----------
      
      procedure Union (Input_Set: in Sets.Set) is
      begin
         Master.Union (Input_Set);
      end Union;
      
      ------------
      -- Modify --
      ------------
      
      procedure Modify
        (Match  : in Element_Type;
         Process: not null access procedure (Item: in out Element_Type))
      is 
         I: constant Sets.Cursor := Master.Find (Match);
         E: Element_Type := Master(I);
      begin
         Process (E);
         Master.Replace_Element (Position => I,
                                 New_Item => E);

      end Modify;
      
      ------------
      -- Delete --
      ------------
      
      procedure Delete (Item: in Element_Type) is
      begin
         Master.Delete (Item);
      end Delete;
   
   end Protected_Master;

   ----------------------
   -- Contains_Element --
   ----------------------
   
   function Contains_Element (Match: Element_Type) return Boolean
     is (Protected_Master.Contains_Element (Match));
   
   ---------------------
   -- Extract_Element --
   ---------------------
   
   function Extract_Element (Match: Element_Type) return Element_Type
     is (Protected_Master.Extract_Element (Match));
      
   --------------------
   -- Extract_Subset --
   --------------------
   
   function Extract_Subset 
     (Filter: not null access function (Element: Element_Type)
                                       return Boolean)
     return Sets.Set
     is (Protected_Master.Extract_Subset (Filter));
   
   --------------------------------------------------
   function Extract_Subset (Match_Set: Sets.Set) return Sets.Set is
     (Protected_Master.Extract_Subset (Match_Set));
   
   -----------------
   -- Extract_Set --
   -----------------
   
   function Extract_Set return Sets.Set is (Protected_Master.Extract_Set);
   
   ---------------
   -- Is_Subset --
   ---------------
   
   function Is_Subset (Query_Set: Sets.Set) return Boolean is 
     (Protected_Master.Is_Subset (Query_Set));
   
   ------------
   -- Insert --
   ------------
   
   procedure Insert (New_Item: in     Element_Type;
                     Inserted:    out Boolean)
   is begin
      Protected_Master.Insert (New_Item => New_Item,
                               Inserted => Inserted);
   end Insert;
   
   -------------
   -- Include --
   -------------
   
   procedure Include (New_Item: in Element_Type) is
   begin
      Protected_Master.Include (New_Item);
   end Include;
   
   --------------------
   -- Include_Subset --
   --------------------
   
   procedure Include_Subset (New_Items: in Sets.Set) is
   begin
      Protected_Master.Include_Subset (New_Items);
   end Include_Subset;
     
   
   -------------
   -- Replace --
   -------------
   
   procedure Replace (New_Item: in Element_Type) is
   begin
      Protected_Master.Replace (New_Item);
   end Replace;
   
   -----------
   -- Union --
   -----------
   
   procedure Union (Input_Set: in Sets.Set) is
   begin
      Protected_Master.Union (Input_Set);
   end Union;
   
   ------------
   -- Modify --
   ------------
   
   procedure Modify 
     (Match  : in Element_Type;
      Process: not null access procedure (Item: in out Element_Type))
   is begin
      Protected_Master.Modify (Match   => Match,
                               Process => Process);
   end Modify;
   
   --------------------
   -- Delete_Element --
   --------------------
   
   procedure Delete_Element (Match: in Element_Type) is
   begin
      Protected_Master.Delete (Match);
   end Delete_Element;
   
   
end Registrar.Protected_Set;
