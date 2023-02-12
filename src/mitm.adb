with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Epoll;
with Mini;

package body MITM is
   use Ada.Streams;
   use GNAT.Sockets;

   Upstream_Addr : constant Sock_Addr_Type :=
      (Addr    => Addresses (Get_Host_By_Name ("chat.protohackers.com"), 1),
       Port    => 16963,
       others  => <>);

   EP : constant Epoll.Epoll_Descriptor := Epoll.Create;

   function "<" (Left, Right : Socket_Type)
      return Boolean
   is (To_C (Left) < To_C (Right));

   package Socket_String_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Socket_Type,
       Element_Type  => Unbounded_String);

   Buffers : Socket_String_Maps.Map := Socket_String_Maps.Empty_Map;

   package Socket_Socket_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Socket_Type,
       Element_Type  => Socket_Type);

   Client_To_Upstream : Socket_Socket_Maps.Map := Socket_Socket_Maps.Empty_Map;
   Upstream_To_Client : Socket_Socket_Maps.Map := Socket_Socket_Maps.Empty_Map;

   function To_String
      (SEA : Stream_Element_Array)
      return String
   is
      Item : String (1 .. Natural (SEA'Length))
         with Address => SEA'Address;
   begin
      return Item;
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

   procedure Write
      (Socket : GNAT.Sockets.Socket_Type;
       Text   : String)
   is
      Item  : constant Stream_Element_Array := To_SEA (Text);
      First : Stream_Element_Offset := Item'First;
      Last  : Stream_Element_Offset;
   begin
      Put_Line ("<<< " & Socket'Image & " " & Text);
      loop
         GNAT.Sockets.Send_Socket (Socket, Item (First .. Item'Last), Last);
         exit when Last >= Item'Last;
         First := Last + 1;
      end loop;
   end Write;

   procedure On_Connect
      (Socket : GNAT.Sockets.Socket_Type;
       Addr   : GNAT.Sockets.Sock_Addr_Type)
   is
      use Socket_Socket_Maps;
      use Socket_String_Maps;
      Upstream : Socket_Type;
      Empty_Buffer : constant Unbounded_String := To_Unbounded_String ("");
   begin
      Put ("Connection from ");
      Put (GNAT.Sockets.Image (Addr));
      New_Line;

      Create_Socket (Upstream);
      Connect_Socket (Upstream, Upstream_Addr);
      Put_Line ("Upstream connected to socket " & Upstream'Image);

      Insert (Buffers, Socket, Empty_Buffer);
      Insert (Buffers, Upstream, Empty_Buffer);
      Insert (Client_To_Upstream, Socket, Upstream);
      Insert (Upstream_To_Client, Upstream, Socket);
      Mini.Subscribe (EP, Upstream);
   end On_Connect;

   Tony : constant String := "7YWHMfk9JZe0LM0g1ZauHuiSxhI";

   function Rewrite_Boguscoin
      (Text : String)
      return String
   is
      function Parse_Address
         (Sub : String)
         return Natural
      is
         I  : Integer := Sub'First;
         Length : Natural := 0;
         Ch : Character;
      begin
         while I <= Sub'Last loop
            Ch := Sub (I);
            case Ch is
               when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' =>
                  I := I + 1;
                  Length := Length + 1;
               when others =>
                  exit;
            end case;
         end loop;

         if Length in 26 .. 35 and then (I = Sub'Last or else Sub (I) = ' ') then
            return Length;
         else
            return 0;
         end if;
      end Parse_Address;

      Previous : Character := ASCII.NUL;
      Ch : Character;
      Result : Unbounded_String;
      Length : Natural;
      I : Integer := Text'First;
   begin
      Put_Line ("Text=" & Text);
      while I <= Text'Last loop
         Ch := Text (I);
         if (Previous = ASCII.NUL or else Previous = ' ') and then Ch = '7' then
            Length := Parse_Address (Text (I .. Text'Last));
            if Length > 0 then
               I := I + Length;
               Append (Result, Tony);
               Previous := Tony (Tony'Last);
            else
               I := I + 1;
               Append (Result, Ch);
               Previous := Ch;
            end if;
         else
            I := I + 1;
            Append (Result, Ch);
            Previous := Ch;
         end if;
      end loop;
      return To_String (Result);
   end Rewrite_Boguscoin;

   procedure On_Receive_Line
      (Socket : GNAT.Sockets.Socket_Type;
       Text   : String)
   is
      use Socket_Socket_Maps;
      Remote : Socket_Type;
   begin
      if Contains (Client_To_Upstream, Socket) then
         --  this is an incoming connection to the proxy
         Remote := Element (Client_To_Upstream, Socket);
         Write (Remote, Rewrite_Boguscoin (Text));
      else
         Remote := Element (Upstream_To_Client, Socket);
         Write (Remote, Rewrite_Boguscoin (Text));
      end if;
   end On_Receive_Line;

   procedure Teardown
      (Socket : GNAT.Sockets.Socket_Type)
   is
      use Socket_Socket_Maps;
      use Socket_String_Maps;
      Remote : Socket_Type;
   begin
      if Contains (Client_To_Upstream, Socket) then
         Remote := Element (Client_To_Upstream, Socket);
         Put_Line ("Closing connection to upstream");
      else
         Remote := Element (Upstream_To_Client, Socket);
         Put_Line ("Closing connection to client");
      end if;

      --  Close_Socket (Socket);
      Delete (Buffers, Socket);

      Close_Socket (Remote);
      Delete (Buffers, Remote);

      Delete (Client_To_Upstream, Socket);
      Delete (Upstream_To_Client, Remote);
   end Teardown;

   procedure On_Receive
      (Socket : GNAT.Sockets.Socket_Type;
       Item   : Ada.Streams.Stream_Element_Array;
       Last   : Ada.Streams.Stream_Element_Offset)
   is
      use Socket_String_Maps;
      Text : constant String := To_String (Item (Item'First .. Last));
      EOL  : Natural;
   begin
      declare
         Buffer : constant Reference_Type := Reference (Buffers, Socket);
      begin
         Append (Buffer, Text);
         loop
            EOL := Index (Buffer, "" & ASCII.LF);
            exit when EOL = 0;
            On_Receive_Line (Socket, Slice (Buffer, 1, EOL));
            Delete (Buffer, 1, EOL);
         end loop;
      end;

      if Last = 0 then
         Teardown (Socket);
      end if;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.Sockets.Close_Socket (Socket);
         Teardown (Socket);
   end On_Receive;

   procedure Run is
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
end MITM;
