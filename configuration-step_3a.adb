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

with Ada.Containers;
with Ada.Characters.Conversions;

with Specification_Scanner;  use Specification_Scanner;

separate (Configuration)

procedure Step_3a (Target: in out Subsystem) is
   
   use type Ada.Containers.Count_Type;
   
   package ACC renames Ada.Characters.Conversions;
   package Subsystems renames Registrar.Subsystems;

   Config_Tree: Declaration_Trees.Tree;
   
   -- We now expect the Configuration unit to be present
   Config_Unit: constant Library_Unit 
     := Reg_Qs.Lookup_Unit (Config_Unit_Name (Target));
   
   
   -- Utilities
   
   function Find_In_Branch (Root: in Declaration_Trees.Cursor;
                            Name: in Wide_Wide_String)
                           return Declaration_Trees.Cursor;
   -- Searches for an entity Name within the branch rooted at Root - i.e.
   -- only within the immediate children of Root
   
   function Find_Package_In_Branch (Root: in Declaration_Trees.Cursor;
                                    Name: in Wide_Wide_String)
                                   return Declaration_Trees.Cursor;
   -- Invokes Find_In_Branch for Root and Name, and then asserts that
   -- the entity (if any) denoted by Name is a non-generic package
                                     
   
   procedure Load_Names (Package_Root: in     Declaration_Trees.Cursor;
                         List        : in out Subsystems.Configuration_Vector);
   -- Load names expected to be given the root cursor of a package entity.
   -- Load names then iterates over the immediate children of that package
   -- entity, isolating constant String objects, and appending their names
   -- to List.
   
   -- Stages
   
   procedure Process_Build;
   procedure Process_Ada_Package (Build_Root: in Declaration_Trees.Cursor);
   procedure Process_C_Package   (Build_Root: in Declaration_Trees.Cursor);
   
   procedure Process_Codepaths;
   procedure Process_Information;
   
   --------------------
   -- Find_In_Branch --
   --------------------
   
   function Find_In_Branch (Root: in Declaration_Trees.Cursor;
                            Name: in Wide_Wide_String)
                           return Declaration_Trees.Cursor
   is
      use Declaration_Trees;
      
      Index: Cursor := First_Child (Root);
   begin
      while Index /= No_Element loop
         if WWU.To_Wide_Wide_String (Config_Tree(Index).Name) = Name then
            return Index;
         end if;
         
         Index := Next_Sibling (Index);
      end loop;
      
      return No_Element;
   end Find_In_Branch;
   
   ----------------------------
   -- Find_Package_In_Branch --
   ----------------------------
   
   function Find_Package_In_Branch (Root: in Declaration_Trees.Cursor;
                                    Name: in Wide_Wide_String)
                                   return Declaration_Trees.Cursor
   is
      use Declaration_Trees;
      
      PC: constant Cursor := Find_In_Branch (Root, Name);
   begin
      if PC = No_Element then
         return PC;
      end if;
      
      declare
         PE: Declared_Entity renames Config_Tree(PC);
      begin
         Assert (Check => PE.Kind = Package_Declaration
                          and then not PE.Is_Generic,
                 Message => ACC.To_String (WWU.To_Wide_Wide_String (PE.Name)) 
                   & " shall be a non-generic package declaration." );
         return PC;
      end;
   end Find_Package_In_Branch;
   
   ----------------
   -- Load_Names --
   ----------------
   
   procedure Load_Names (Package_Root: in     Declaration_Trees.Cursor;
                         List        : in out Subsystems.Configuration_Vector)
   is
      use Declaration_Trees;
      
      Index: Cursor := First_Child (Package_Root);
   begin
      while Index /= No_Element loop
         declare
            Ent: Declared_Entity renames Config_Tree(Index);
         begin
            if Ent.Kind = Object_Declaration
              and then Ent.Is_Constant
              and then WWU.To_Wide_Wide_String (Ent.Subtype_Mark) = "string"
            then
               List.Append ((Name  => Ent.Name, 
                             Value => UBS.Null_Unbounded_String));
            end if;
            
            Index := Next_Sibling (Index);
         end;
      end loop;
   end Load_Names;
   
   -------------------
   -- Process_Build --
   -------------------
   
   procedure Process_Build is
      use Declaration_Trees;
      
      Build_Root: constant Cursor 
        := Find_Package_In_Branch (Root => First_Child (Config_Tree.Root),
                                   Name => "build");
   begin
      if Build_Root = No_Element then
         return;
      end if;
      
      -- Load the External_Libraries, if present
      declare
         P: constant Cursor 
           := Find_Package_In_Branch (Root => Build_Root,
                                      Name => "external_libraries");
      begin
         if P /= No_Element then
            Load_Names 
              (Package_Root => P,
               List         => Target.Configuration.External_Libraries);
         end if;
      end;

      Process_Ada_Package (Build_Root);
      Process_C_Package   (Build_Root);
   end Process_Build;
   
   -------------------------
   -- Process_Ada_Package --
   -------------------------
   
   procedure Process_Ada_Package (Build_Root: in Declaration_Trees.Cursor) is
      use Declaration_Trees;
      
      Ada_Root: constant Cursor
        := Find_Package_In_Branch (Root => Build_Root,
                                   Name => "ada");
      
      Comp_Opt: Cursor; 

   begin
      if Ada_Root /= No_Element then
         
         Comp_Opt := Find_Package_In_Branch (Root => Ada_Root,
                                             Name => "compiler_options");
         
         if Comp_Opt /= No_Element then
            Load_Names (Package_Root => Comp_Opt,
                        List         => Target.Configuration.Ada_Compiler_Opts);
         end if;
      end if;
   end Process_Ada_Package;
   
   -----------------------
   -- Process_C_Package --
   -----------------------
   
   procedure Process_C_Package (Build_Root: in Declaration_Trees.Cursor) is
      use Declaration_Trees;
      
      C_Root: constant Cursor
        := Find_Package_In_Branch (Root => Build_Root,
                                   Name => "c");
      
      Comp_Opt: Cursor;
      CPP_Defs: Cursor;

   begin
      if C_Root /= No_Element then
         
         Comp_Opt := Find_Package_In_Branch (Root => C_Root,
                                             Name => "compiler_options");
         
         if Comp_Opt /= No_Element then
            Load_Names (Package_Root => Comp_Opt,
                        List         => Target.Configuration.C_Compiler_Opts);
         end if;
         
         CPP_Defs 
           := Find_Package_In_Branch (Root => C_Root,
                                      Name => "preprocessor_definitions");
         
         if CPP_Defs /= No_Element then
            Load_Names (Package_Root => CPP_Defs,
                        List         => Target.Configuration.C_Definitions);
         end if;
         
      end if;
   end Process_C_Package;
   
   -----------------------
   -- Process_Codepaths --
   -----------------------
   
   procedure Process_Codepaths is
      use Declaration_Trees;
      
      CP_Root: constant Cursor 
        := Find_Package_In_Branch (Root => First_Child (Config_Tree.Root),
                                   Name => "codepaths");
      
   begin
      if CP_Root /= No_Element then
         Assert (Check => Target.Name.To_String /= "aura",
                 Message => "Root Configuration (AURA.Root) shall not "
                   &        "have a codepaths package");
         
         Load_Names (Package_Root => CP_Root,
                     List         => Target.Configuration.Codepaths);
      end if;
   end Process_Codepaths;
   
   -------------------------
   -- Process_Information --
   -------------------------
   
   procedure Process_Information is
      use Declaration_Trees;
      
      Info_Root: constant Cursor 
        := Find_Package_In_Branch (Root => First_Child (Config_Tree.Root),
                                   Name => "information");
   begin
      if Info_Root /= No_Element then
         Load_Names (Package_Root => Info_Root,
                     List         => Target.Configuration.Codepaths);
      end if;
   end Process_Information;
   
begin
   Target.Configuration
     := (others => Subsystems.Configuration_Vectors.Empty_Vector);
   
   Scan_Package_Spec (Unit      => Config_Unit,
                      Unit_Tree => Config_Tree);
   
   Process_Build;       -- Includes Ada and C packages
   Process_Codepaths;
   Process_Information;
   
   -- Now we have a sense of which packages are in the configuration unit, and
   -- which objects need to be exported from the loader program, executed in
   -- Step 3b
   
   -- However if we *know* we don't have anything to load, then we can skip
   -- right to step 4. This can happen when the configuration file is empty.
   
   if         Target.Configuration.External_Libraries.Length = 0
     and then Target.Configuration.Ada_Compiler_Opts.Length  = 0
     and then Target.Configuration.C_Compiler_Opts.Length    = 0
     and then Target.Configuration.C_Definitions.Length      = 0
     and then Target.Configuration.Codepaths.Length          = 0
     and then Target.Configuration.Information.Length        = 0
   then
      Step_4 (Target);
   else
      Step_3b (Target);
   end if;
   
end Step_3a;
