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

with Ada.Containers;
with Ada.Strings.Unbounded;

with Registrar.Registry;
with Registrar.Registration;
with Registrar.Subsystems;
with Registrar.Executive.Subsystems_Request;

package body Registrar.Executive.Library_Units_Request is
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Library_Units_Request_Order)
                  return String
   is 
      use Ada.Strings.Unbounded;
      use type Ada.Containers.Count_Type;
      
      Image_String: Unbounded_String;
   begin
      Set_Unbounded_String 
        (Target => Image_String,
         Source => "[Library_Units_Request_Order]" & New_Line 
           &       "Requested Units:");
      
      if Order.Requested_Units.Length = 0 then
         Append (Source => Image_String,
                 New_Item => " NONE");
      else
         for Unit of Order.Requested_Units loop
            Append (Source   => Image_String,
                    New_Item => New_Line & "- " & Unit.Name.To_UTF8_String);
         end loop;
      end if;
      
      return To_String (Image_String);
   end Image;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Library_Units_Request_Order) is
      use Unit_Names;
      use Registrar.Subsystems;
      use Registrar.Library_Units;
      use Registrar.Executive.Subsystems_Request;
      
      package All_Library_Units renames Registrar.Registry.All_Library_Units;
      
      SS_Req_Order: Subsystems_Request_Order;
      SS_Reqs     : Subsystem_Sets.Set 
        renames SS_Req_Order.Requested_Subsystems;
      
      Requested_Units: Library_Unit_Sets.Set renames Order.Requested_Units;

   begin
      pragma Assert (not Order.Requested_Units.Is_Empty);
      
      SS_Req_Order.Tracker := Registration.Entry_Progress'Access;
      
      -- Build the Subsystem request set
      
      for Unit of Order.Requested_Units loop
         pragma Assert (Unit.State = Requested);

         declare
            SS_Request: Subsystem (AURA => True);
         begin
            SS_Request.Name  := Unit_Name (Unit.Name.Subsystem_Name);
            SS_Request.State := Requested;
            SS_Reqs.Include (SS_Request);
         end;
      end loop;
      
      -- Submit the subsystem requests
      SS_Req_Order.Tracker.Increment_Total_Items;
      Workers.Enqueue_Order (SS_Req_Order);
      
      -- Include the unit requests
      All_Library_Units.Union (Requested_Units);
      
   end Execute;
   
end Registrar.Executive.Library_Units_Request;
