with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;
with GNAT.Sockets;
with Epoll;
with Mini;

procedure Miniserver is
   EP : constant Epoll.Epoll_Descriptor := Mini.Bind ("", "3000");

   procedure On_Connect
      (Socket : GNAT.Sockets.Socket_Type;
       Addr   : GNAT.Sockets.Sock_Addr_Type)
   is
      pragma Unreferenced (Socket);
   begin
      Put ("Connection from ");
      Put (GNAT.Sockets.Image (Addr));
      New_Line;
   end On_Connect;

   procedure On_Receive
      (Socket : GNAT.Sockets.Socket_Type;
       Item   : Ada.Streams.Stream_Element_Array;
       Last   : Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Socket);
      Text : String (1 .. Natural (Last))
         with Import, Address => Item'Address;
   begin
      Put_Line (Text);
   end On_Receive;
begin
   loop
      Mini.Serve (EP, On_Connect'Unrestricted_Access, On_Receive'Unrestricted_Access, 32);
   end loop;
end Miniserver;
