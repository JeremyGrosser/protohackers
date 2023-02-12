with Ada.Streams;
with GNAT.Sockets;
with Epoll;

package Mini is
   Listen_Backlog : constant := 128;
   Receive_Length : constant := 65536;

   procedure Bind
      (EP   : Epoll.Epoll_Descriptor;
       Name : String;
       Port : String);
   --  Resolves Name and Port into local addresses, creates a socket for each
   --  address, calls bind and listen, then adds the socket to an epoll
   --  descriptor. Returns a single epoll descriptor with all the sockets
   --  added. The Event.Data field is populated with the socket number.

   type Connect_Callback is not null access procedure
      (Socket : GNAT.Sockets.Socket_Type;
       Addr   : GNAT.Sockets.Sock_Addr_Type);

   type Receive_Callback is not null access procedure
      (Socket : GNAT.Sockets.Socket_Type;
       Item   : Ada.Streams.Stream_Element_Array;
       Last   : Ada.Streams.Stream_Element_Offset);

   procedure Serve
      (EP         : Epoll.Epoll_Descriptor;
       On_Connect : Connect_Callback;
       On_Receive : Receive_Callback;
       Max_Events : Positive);

   procedure Subscribe
      (EP      : Epoll.Epoll_Descriptor;
       Socket  : GNAT.Sockets.Socket_Type);

end Mini;
