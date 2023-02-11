with Ada.Unchecked_Conversion;
with Interfaces;

package body Mini is

   type Event_Context is record
      Is_Server : Boolean;
      Socket    : GNAT.Sockets.Socket_Type;
   end record
      with Size => 64;

   function From_Event_Context is new Ada.Unchecked_Conversion
      (Event_Context, Interfaces.Unsigned_64);
   function To_Event_Context is new Ada.Unchecked_Conversion
      (Interfaces.Unsigned_64, Event_Context);

   procedure Bind
      (EP   : Epoll.Epoll_Descriptor;
       Name : String;
       Port : String)
   is
      use GNAT.Sockets;
      use Interfaces;
      use Epoll;

      Addrs  : constant Address_Info_Array := Get_Address_Info (Name, Port, Passive => True);
      Socket : Socket_Type;
      Event  : aliased Epoll_Event :=
         (Data => -1,
          Flags =>
            (Readable  => True,
             Exclusive => True,
             others    => False));
   begin
      for Addr of Addrs loop
         if Name = "" and then Addr.Addr.Family = Family_Inet then
            null;
         else
            Create_Socket (Socket, Addr.Addr.Family, Addr.Mode, Addr.Level);
            Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
            Set_Socket_Option (Socket, IP_Protocol_For_TCP_Level, (No_Delay, True));
            Bind_Socket (Socket, Addr.Addr);
            Listen_Socket (Socket, Listen_Backlog);
            Event.Data := From_Event_Context
               (Event_Context'(Is_Server => True, Socket => Socket));
            Control (EP, Socket, Add, Event'Access);
         end if;
      end loop;
   end Bind;

   procedure Serve
      (EP : Epoll.Epoll_Descriptor;
       On_Connect : Connect_Callback;
       On_Receive : Receive_Callback;
       Max_Events : Positive)
   is
      use GNAT.Sockets;
      use Ada.Streams;
      use Epoll;

      Client_Event : aliased Epoll_Event :=
         (Data => 0,
          Flags =>
            (Readable => True,
             others   => False));

      Client   : Socket_Type;
      Addr     : Sock_Addr_Type;
      Context  : Event_Context;

      Data : Stream_Element_Array (1 .. Receive_Length);
      Last : Stream_Element_Offset;
   begin
      for Event of Epoll.Wait (EP, Max_Events) loop
         Context := To_Event_Context (Event.Data);
         if Context.Is_Server and then Event.Flags.Readable then
            Accept_Socket (Context.Socket, Client, Addr);
            On_Connect.all (Client, Addr);

            Client_Event.Data := From_Event_Context (Event_Context'
               (Socket     => Client,
                Is_Server  => False));
            Control (EP, Client, Add, Client_Event'Access);
         elsif not Context.Is_Server then
            if Event.Flags.Peer_Shutdown or else Event.Flags.Error or else Event.Flags.Hang_Up then
               Close_Socket (Context.Socket);
            elsif Event.Flags.Readable then
               loop
                  Receive_Socket (Context.Socket, Data, Last);
                  On_Receive.all (Context.Socket, Data, Last);
                  if Last = 0 then
                     Close_Socket (Context.Socket);
                  end if;
                  exit when Last < Data'Last;
               end loop;
            end if;
         end if;
      end loop;
   end Serve;
end Mini;
