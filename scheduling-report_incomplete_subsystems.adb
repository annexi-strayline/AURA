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

with Unit_Names.Hash;

separate (Scheduling)

procedure Report_Incomplete_Subsystems
   (Requested_Units: in Registrar.Library_Units.Library_Unit_Sets.Set)
is
   Verbose_Output: constant Boolean := Parameters.Output_Style = Verbose;
   
   subtype Count_Type is Ada.Containers.Count_Type;
   
   package Unfulfilled_Subsystem_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unit_Names.Unit_Name,
      Element_Type    => Unit_Names.Sets.Set,
      Hash            => Unit_Names.Hash,
      Equivalent_Keys => Unit_Names."=",
      "="             => Unit_Names.Sets."=");
   -- Maps a subsystem name (key) to a set of unit names for all missing
   -- (requested) units associated with the subsystem
   
   Missing_Map: Unfulfilled_Subsystem_Maps.Map;
   
   procedure Dump_Reverse_Dependency_Trace 
     (Set_Cursor: in Unfulfilled_Subsystem_Maps.Cursor)
   is begin
      -- We need to dump a listing of all reverse dependencies associated
      -- with each missing unit of the set mapped to via Set_Cursor
      
      for Missing_Unit_Name of Missing_Map(Set_Cursor).Element.all loop
         declare
            Rev_Deps: constant Unit_Names.Sets.Set
              := Registrar.Queries.Dependent_Units (Missing_Unit_Name);
         begin
            if not Rev_Deps.Is_Empty then
               UI.Put_Empty_Tag;
               Put_Line ("   - " & Missing_Unit_Name.To_UTF8_String 
                           & ", which is with'ed by:");
            end if;
            
            for Rev_Dep_Name of Rev_Deps loop
               UI.Put_Empty_Tag;
               Put_Line ("       -> " & Rev_Dep_Name.To_UTF8_String);
            end loop;
         end;
      end loop;
   end Dump_Reverse_Dependency_Trace;
   
begin

   -- Some units are missing. We will build a map which links each
   -- missing unit with the subsystem that is missing that subsystem
   
   declare
      use Unfulfilled_Subsystem_Maps;
      
      Associated_Subsys_Name: Unit_Names.Unit_Name;
      Mapped_Subsys : Cursor;
   begin
      
      for Missing_Unit of Requested_Units loop
         Associated_Subsys_Name := Missing_Unit.Name.Subsystem_Name;
         
         Mapped_Subsys := Missing_Map.Find (Associated_Subsys_Name);
         
         if Mapped_Subsys = No_Element then
            Missing_Map.Insert
              (Key      => Associated_Subsys_Name,
               New_Item => Unit_Names.Sets.To_Set (Missing_Unit.Name));
         else
            Missing_Map(Mapped_Subsys).Insert (Missing_Unit.Name);
         end if;
      end loop;
   end;
   
   for C in Missing_Map.Iterate loop
      UI.Put_Fail_Tag;
      Put_Line (" Subsystem """ 
                  & Unfulfilled_Subsystem_Maps.Key(C).To_UTF8_String
                  & """ is missing" 
                  & Count_Type'Image (Missing_Map(C).Length)
                  & " expected units"
                  & (if Verbose_Output then ':' else '.'));
      
      if Verbose_Output then
         Dump_Reverse_Dependency_Trace (C);
      end if;
   end loop;
   
   if not Verbose_Output then
      UI.Put_Info_Tag;
      Put_Line (" Use -v to see dependency details of missing units.");
   end if;
   
end Report_Incomplete_Subsystems;
