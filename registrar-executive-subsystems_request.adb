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

package body Registrar.Executive.Subsystems_Request is
   
   -----------
   -- Image --
   -----------
   
   function Image (Order: Subsystems_Request_Order)
                  return String
   is 
      use Ada.Strings.Unbounded;
      use type Ada.Containers.Count_Type;
      
      Image_String: Unbounded_String;
   begin
      Set_Unbounded_String 
        (Target => Image_String,
         Source => "[Subsystems_Request_Order]" & New_Line 
           &       "Requested Subsystems:");
      
      if Order.Requested_Subsystems.Length = 0 then
         Append (Source   => Image_String,
                 New_Item => " NONE");
      else
         for SS of Order.Requested_Subsystems loop
            Append (Source   => Image_String,
                    New_Item => New_Line & "- " & SS.Name.To_UTF8_String);
         end loop;
      end if;
      
      return To_String (Image_String);
   end Image;
   
   -------------
   -- Execute --
   -------------
   
   procedure Execute (Order: in out Subsystems_Request_Order) is
      package All_Subsystems renames Registrar.Registry.All_Subsystems;
      
   begin
      pragma Assert (for all SS of Order.Requested_Subsystems 
                       => SS.AURA and then SS.State = Requested);
      
      All_Subsystems.Union (Order.Requested_Subsystems);
      
   end Execute;
   
end Registrar.Executive.Subsystems_Request;
