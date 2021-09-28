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

with Registrar.Dependency_Maps;

separate (Scheduling)

procedure Report_Unavailable_Subsystems 
  (Unavailable_Subsystems: in Registrar.Subsystems.Subsystem_Sets.Set)
is 
   Verbose_Output: constant Boolean := Parameters.Output_Style = Verbose;
   
   use type Unit_Names.Unit_Name;
   package UBS renames Registrar.Subsystems.UBS;
   
   procedure Dump_Dependency_Trace
     (Target: Registrar.Subsystems.Subsystem)
   is begin
      -- First collect a map of all units
      
      -- Iterate over Target's reverse dependencies, and scan the
      -- dependencies of each unit of each dependent subsystem,
      -- dumping the specfic unit withs of units in the target subsystem
      
        Dependent_Subsystems_Iteration:
      for Dep_Subsys_Name of 
        Registrar.Queries.Dependent_Subsystems (Target.Name)
      loop
         UI.Put_Empty_Tag;
         Put_Line ("   Subsystem "
                & Dep_Subsys_Name.To_UTF8_String 
                & " has dependent units:");
         
         declare
            -- First collect a map with a key for each  unit name  that is a
            -- member of the dependent subsystem, and then inserting all
            -- reverse dependencies of each of those units that is a member
            -- of the Target subsystem
            
            use Registrar.Subsystems;
            use Registrar.Library_Units;
            
            Subsys_Units: constant Library_Unit_Sets.Set
              := Registrar.Queries.Subsystem_Library_Units 
                (Subsystem'(Name => Dep_Subsys_Name, others => <>));
            
            Target_Rev_Deps: Registrar.Dependency_Maps.Map;
            
         begin
            -- For each unit of this subsystem, check for any reverse
            -- dependencies that are members of Target. If we find that,
            -- we output that information
            
            for Unit of Subsys_Units loop
               declare
                  Unit_Deps: constant Unit_Names.Sets.Set
                    := Registrar.Queries.Unit_Dependencies (Unit.Name);
                  Target_Deps: Unit_Names.Sets.Set;
               begin
                  for Name of Unit_Deps loop
                     if Name.Subsystem_Name = Target.Name then
                        Target_Deps.Insert (Name);
                     end if;
                  end loop;
                  
                  if not Target_Deps.Is_Empty then
                     UI.Put_Empty_Tag;
                     Put_Line ("     - "
                                 & Unit.Name.To_UTF8_String 
                                 & " withs:");
                     for Target_Unit_Name of Target_Deps loop
                        UI.Put_Empty_Tag;
                        Put_Line ("        -> "
                                    & Target_Unit_Name.To_UTF8_String);
                     end loop;
                  end if;
               end;
            end loop;
         end;
         
         New_Line;
      end loop Dependent_Subsystems_Iteration;
      
   end Dump_Dependency_Trace;
   
begin
   -- Note that this is always called following a failed Checkout_Cycle,
   -- which has already alerted the user that subsystems could not be
   -- checked-out
   
   for Subsys of Unavailable_Subsystems loop
      UI.Put_Fail_Tag;
      Put_Line (' ' 
                  & Subsys.Name.To_UTF8_String 
                  & ": "
                  & UBS.To_String (Subsys.Aquisition_Failure));
      
      if Verbose_Output then
         Dump_Dependency_Trace (Subsys);
      end if;
   end loop;
   
   if not Verbose_Output then
      UI.Put_Info_Tag;
      Put_Line (" Use -v to see dependency details of missing subsystems.");
   end if;
   
end Report_Unavailable_Subsystems;
