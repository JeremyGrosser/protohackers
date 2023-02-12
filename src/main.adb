with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Primal;
with Echo;
with Asset;
with Chat;
with KVStore;
with MITM;

procedure Main is
begin
   if Argument_Count < 1 then
      Put_Line (Standard_Error, "Usage: " & Command_Name & " <service>");
      Set_Exit_Status (1);
      return;
   end if;

   if Argument (1) = "echo" then
      Echo.Run;
   elsif Argument (1) = "primal" then
      Primal.Run;
   elsif Argument (1) = "asset" then
      Asset.Run;
   elsif Argument (1) = "chat" then
      Chat.Run;
   elsif Argument (1) = "kvstore" then
      KVStore.Run;
   elsif Argument (1) = "mitm" then
      MITM.Run;
   else
      Put_Line (Standard_Error, "Unknown service: " & Argument (1));
      Set_Exit_Status (1);
   end if;
end Main;
