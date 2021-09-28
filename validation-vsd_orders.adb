------------------------------------------------------------------------------
--                                                                          --
--                     Ada User Repository Annex (AURA)                     --
--                ANNEXI-STRAYLINE Reference Implementation                 --
--                                                                          --
--                        Command Line Interface                            --
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

with Ada.Assertions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Unit_Names.Sets;
with Workers.Reporting;

separate (Validation)

package body VSD_Orders is
   
   package UBS renames Ada.Strings.Unbounded;
   New_Line: Character renames Workers.Reporting.New_Line;
   
   -----------
   -- Image --
   -----------
   
   function  Image (Order: Validate_Subsystem_Dependencies_Order)
                   return String
   is 
      Image_String: Bounded_String := UBS.To_Unbounded_String
        (    "[Validate_Subsystem_Dependencies_Order] (Validation)" & New_Line
           & "Subsystems to be checked:");
      
      procedure Append (Source  : in out UBS.Unbounded_String := Image_String;
                        New_Item: in     String)
        renames UBS.Append;
   begin
      for SS of Order.Check_Subset loop
         Append (New_Line & "- " & SS.Name.To_UTF8_String);
      end loop;
      
      return UBS.To_String (Image_String);
   end Image;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Validate_Subsystem_Dependencies_Order) is
      use Repositories;
      use Registrar.Subsystems;
      
      Dependencies: constant Subsystem_Sets.Set 
        := Registrar.Queries.Subsystem_Dependencies (Order.Target);
      
      Failed_Set: Subsystem_Sets.Set;
      
      procedure Fail is
         
         Fail_Message: UBS.Bounded_String := UBS.To_Unbounded_String
           (    "Subsystem """ & Order.Target.Name.To_UTF8_String & """ "
              & " was checked out from a " & New_Line
              & " System Repository (Repository"
              & Repository_Index'Image (Order.Target.Source_Repository) 
              & "). Subsystems checked-out from System Repositories" 
              & New_Line 
              & "must only depend on other subsystems from the same repository." 
              & New_Line 
              & "The offending dependencies, and their repository index, "
              & "are as follows: ");
         
         procedure Append (Source  : in out UBS.Unbounded_String := Fail_Message;
                           New_Item: in     String)
           renames UBS.Append;
         
         function Trim (Souce: in Sting;
                        Side : in Ada.Strings.Trim_End := Ada.Strings.Both)
                       return String
           renames Ada.Strings.Fixed.Trim;
         
      begin
         for SS of Failed_Set loop
            Append (SS.Name.To_UTF8_String 
                      & " ("
                      & Trim (Repository_Index'Image (SS.Source_Repository))
                      & ')' & New_Line);
         end loop;
         
         raise Ada.Assertions.Assertion_Error with 
           UBS.To_String (Fail_Message);
      end Fail;

      
   begin
      for SS of Dependencies loop
         if SS.Source_Repository /= Order.Target.Source_Repository then
            Failed_Set.Insert (SS);
         end if;
      end loop;
      
      if not Failed_Set.Is_Empty then Fail; end if;
      
   end Execute;
   
end VSD_Orders;
