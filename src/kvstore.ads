with GNAT.Sockets;
with Ada.Streams;

package KVStore is

   procedure On_Receive
      (Socket : GNAT.Sockets.Socket_Type;
       Addr   : not null access GNAT.Sockets.Sock_Addr_Type;
       Item   : Ada.Streams.Stream_Element_Array;
       Last   : Ada.Streams.Stream_Element_Offset);

   procedure Run;

end KVStore;
