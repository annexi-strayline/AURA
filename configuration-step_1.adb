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

with Registrar.Last_Run;
with Stream_Hashing;

separate (Configuration)

procedure Step_1 (Target: in out Subsystem) is
   use Unit_Names;
   use type Stream_Hashing.Hash_Type;
      
   Last_Run_Units: Registrar.Library_Units.Library_Unit_Sets.Set
     renames Registrar.Last_Run.All_Library_Units;
   
   Last_Run_Subsystems: Registrar.Subsystems.Subsystem_Sets.Set
     renames Registrar.Last_Run.All_Subsystems;
      
   Conf_Unit_Name: constant Unit_Name := Config_Unit_Name (Target);
   
   Root_Config: constant Boolean := Target.Name.To_String = "aura";
   
   Currently_Have_Conf: constant Boolean
     := Reg_Qs.Unit_Entered (Conf_Unit_Name);
   
   Currently_Have_Manifest: constant Boolean
     := (not Root_Config)
       and then Reg_Qs.Unit_Entered (Manifest_Unit_Name (Target));

   Current_Conf_Unit: Library_Unit 
     := (if Currently_Have_Conf then 
            Reg_Qs.Lookup_Unit (Conf_Unit_Name)
         else
           (others => <>));
   
   Previously_Had_Conf: constant Boolean
     := Last_Run_Units.Contains (Current_Conf_Unit);
   
   Last_Run_Conf_Unit: constant Library_Unit
   -- This only matters if we also currently have a configuration unit
     := (if Currently_Have_Conf and then Previously_Had_Conf then
            Last_Run_Units (Last_Run_Units.Find (Current_Conf_Unit))
         else
           (others => <>));
   
begin
   
   if Currently_Have_Conf then
      
      -- See if we can load the last-run data
      if Previously_Had_Conf 
        and then Current_Conf_Unit.Specification_Hash 
        = Last_Run_Conf_Unit.Specification_Hash
        
        and then Current_Conf_Unit.Implementation_Hash
        = Last_Run_Conf_Unit.Implementation_Hash
      then
         -- Nice, this makes things much quicker!
         declare
            use Registrar.Subsystems;
            
            Last_Run_SS: constant Subsystem
              := Last_Run_Subsystems (Last_Run_Subsystems.Find (Target));
         begin
            Target.Configuration := Last_Run_SS.Configuration;
            Step_4 (Target);
            return;
         end;
         
      else
         -- We need to re-parse the configuration unit
         Step_3a (Target);
         return;
      end if;
      
   else
      -- No configuration unit
      
      if Currently_Have_Manifest then
         -- Load the manifest. Note that this will never be true for the
         -- root config since Currently_Have_Manifest will always be false
         Step_2 (Target);
         return;
      else
         -- No manifest or config. We will generate the barebones one,
         -- simple because the checkout spec is a child of the config unit, so
         -- we need something there.
         --
         -- If target is AURA, then this is the root config, and in that case,
         -- it's fine if we don't have any at all - we definately don't want to
         -- generate it. We also don't need to invoke complete since there is
         -- nothing to update in that case
         
         if not Root_Config then
            Generate_Basic_Config (Target);
            Complete (Target);
         end if;
         
         return;
      end if;
   end if;
      
end Step_1;
