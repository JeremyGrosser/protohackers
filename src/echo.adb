with Ada.Text_IO; use Ada.Text_IO;
with Epoll;
with Mini;

package body Echo is

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
      use Ada.Streams;
      First : Stream_Element_Offset := Item'First;
      Sent  : Stream_Element_Offset;
   begin
      loop
         GNAT.Sockets.Send_Socket (Socket, Item (First .. Last), Sent);
         First := First + Sent + 1;
         exit when First >= Last;
      end loop;
   end On_Receive;

   procedure Run is
      EP : constant Epoll.Epoll_Descriptor := Epoll.Create;
   begin
      Mini.Bind (EP, "", "3000");
      loop
         Mini.Serve
            (EP         => EP,
             On_Connect => On_Connect'Access,
             On_Receive => On_Receive'Access,
             Max_Events => 32);
      end loop;
   end Run;

end Echo;
