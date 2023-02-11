with GNAT.Sockets;
with Ada.Streams;

package Chat is

   procedure On_Connect
      (Socket : GNAT.Sockets.Socket_Type;
       Addr   : GNAT.Sockets.Sock_Addr_Type);

   procedure On_Receive
      (Socket : GNAT.Sockets.Socket_Type;
       Item   : Ada.Streams.Stream_Element_Array;
       Last   : Ada.Streams.Stream_Element_Offset);

   procedure Run;

end Chat;
