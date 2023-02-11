with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body KVStore is
   use Ada.Streams;
   use GNAT.Sockets;

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => String);
   use String_Maps;

   DB : String_Maps.Map := String_Maps.Empty_Map;

   function To_String
      (SEA : Stream_Element_Array)
      return String
   is
      S : String (1 .. Natural (SEA'Length))
         with Address => SEA'Address;
   begin
      return S;
   end To_String;

   function To_SEA
      (Text : String)
      return Stream_Element_Array
   is
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Text'Length))
         with Address => Text'Address;
   begin
      return Item;
   end To_SEA;

   procedure Insert
      (Text : String)
   is
      Delimiter : constant Positive := Index (Text, "=");
      Key : constant String := Text (Text'First .. Delimiter - 1);
      Val : constant String := Text (Delimiter + 1 .. Text'Last);
   begin
      if Contains (DB, Key) then
         Delete (DB, Key);
      end if;
      Insert (DB, Key, Val);
      Put_Line ("Insert " & Key & "=" & Val);
   end Insert;

   procedure Query
      (Socket : GNAT.Sockets.Socket_Type;
       Addr   : GNAT.Sockets.Sock_Addr_Type;
       Key    : String)
   is
      Last : Stream_Element_Offset;
   begin
      Put_Line ("Query " & Key);
      if Contains (DB, Key) then
         declare
            Val : constant String := Element (DB, Key);
            Response : constant String := Key & "=" & Val;
         begin
            Send_Socket (Socket, To_SEA (Response), Last, Addr);
            Put_Line (">>> " & Response);
         end;
      end if;
   end Query;

   procedure On_Receive
      (Socket : GNAT.Sockets.Socket_Type;
       Addr   : not null access GNAT.Sockets.Sock_Addr_Type;
       Item   : Ada.Streams.Stream_Element_Array;
       Last   : Ada.Streams.Stream_Element_Offset)
   is
      Text : constant String := To_String (Item (Item'First .. Last));
      Sent : Stream_Element_Offset;
   begin
      if Index (Text, "=") /= 0 then
         Insert (Text);
      elsif Text = "version" then
         Send_Socket (Socket, To_SEA ("version=Ken's Key-Value Store 1.0"), Sent, Addr);
      else
         Query (Socket, Addr.all, Text);
      end if;
   end On_Receive;

   procedure Run is
      Socket : Socket_Type;
      Addr   : constant Sock_Addr_Type := (Addr => Any_Inet_Addr, Port => 3000, others => <>);
      From   : aliased Sock_Addr_Type;
      Item   : Stream_Element_Array (1 .. 1000);
      Last   : Stream_Element_Offset;
   begin
      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Socket, Addr);
      loop
         GNAT.Sockets.Receive_Socket (Socket, Item, Last, From);
         On_Receive (Socket, From'Access, Item, Last);
      end loop;
   end Run;

end KVStore;
