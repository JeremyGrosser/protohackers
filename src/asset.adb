with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Conversion;
with System;
with Epoll;
with Mini;

with Chests.Ring_Buffers;

package body Asset is

   package Stream_Buffers is new Chests.Ring_Buffers
      (Element_Type => Ada.Streams.Stream_Element,
       Capacity     => Mini.Receive_Length * 2);
   Empty_Buffer : Stream_Buffers.Ring_Buffer;

   type Int32 is new Integer;

   package Int32_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Int32,
       Element_Type  => Int32);

   function "<" (Left, Right : GNAT.Sockets.Socket_Type)
      return Boolean
   is (GNAT.Sockets.To_C (Left) < GNAT.Sockets.To_C (Right));

   function "=" (Left, Right : Int32_Maps.Map)
      return Boolean
   is
      use System;
   begin
      return Left'Address = Right'Address;
   end "=";

   function "=" (Left, Right : Stream_Buffers.Ring_Buffer)
      return Boolean
   is
      use System;
   begin
      return Left'Address = Right'Address;
   end "=";

   package Socket_Int32_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => GNAT.Sockets.Socket_Type,
       Element_Type  => Int32_Maps.Map);

   DB : Socket_Int32_Maps.Map := Socket_Int32_Maps.Empty_Map;

   package Socket_Buffer_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => GNAT.Sockets.Socket_Type,
       Element_Type  => Stream_Buffers.Ring_Buffer);

   Buffers : Socket_Buffer_Maps.Map := Socket_Buffer_Maps.Empty_Map;

   function Query
      (This : in out Int32_Maps.Map;
       Time_Start, Time_End : Int32)
       return Int32
   is
      use Int32_Maps;
      Timestamp, Price : Int32;
      Mean     : Long_Long_Integer := 0;
      Count    : Long_Long_Integer := 0;
   begin
      if Time_End < Time_Start then
         return 0;
      end if;

      for C in Iterate (This) loop
         Timestamp := Key (C);
         exit when Timestamp > Time_End;
         if Timestamp >= Time_Start then
            Count := Count + 1;
            Price := Element (C);
            Mean := Mean + Long_Long_Integer (Price);
         end if;
      end loop;

      if Count > 0 then
         return Int32 (Mean / Count);
      else
         return 0;
      end if;
   end Query;

   type Message is record
      Action      : Character;
      Arg_1       : Int32;
      Arg_2       : Int32;
   end record
      with  Size => 9 * 8,
            Bit_Order => System.High_Order_First,
            Scalar_Storage_Order => System.High_Order_First;
   for Message use record
      Action   at 0 range 0 .. 7;
      Arg_1    at 1 range 0 .. 31;
      Arg_2    at 5 range 0 .. 31;
   end record;

   subtype Message_Bytes is Ada.Streams.Stream_Element_Array (1 .. 9);
   function To_Message is new Ada.Unchecked_Conversion
      (Source => Message_Bytes,
       Target => Message);

   type Query_Response is record
      Mean : Int32;
   end record
      with Size => 32,
           Bit_Order => System.High_Order_First,
           Scalar_Storage_Order => System.High_Order_First;
   for Query_Response use record
      Mean at 0 range 0 .. 31;
   end record;

   subtype Query_Response_Bytes is Ada.Streams.Stream_Element_Array (1 .. 4);
   function To_Bytes is new Ada.Unchecked_Conversion
      (Source => Query_Response,
       Target => Query_Response_Bytes);

   procedure On_Connect
      (Socket : GNAT.Sockets.Socket_Type;
       Addr   : GNAT.Sockets.Sock_Addr_Type)
   is
      use Socket_Buffer_Maps;
      use Socket_Int32_Maps;
   begin
      Put ("Connection from ");
      Put (GNAT.Sockets.Image (Addr));
      New_Line;

      if Contains (DB, Socket) then
         Int32_Maps.Clear (Reference (DB, Socket));
      else
         Insert (DB, Socket, Int32_Maps.Empty_Map);
      end if;

      if Contains (Buffers, Socket) then
         Stream_Buffers.Clear (Reference (Buffers, Socket));
      else
         Insert (Buffers, Socket, Empty_Buffer);
      end if;
   end On_Connect;

   procedure On_Message
      (Socket  : GNAT.Sockets.Socket_Type;
       M       : Message;
       Error   : out Boolean)
   is
      use Socket_Int32_Maps;
      use GNAT.Sockets;
      use Ada.Streams;
      Last     : Stream_Element_Offset;
      Response : Query_Response;
   begin
      Error := False;
      Put (Socket'Image);
      Put_Line (M'Image);
      case M.Action is
         when 'I' =>
            if Int32_Maps.Contains (Reference (DB, Socket), M.Arg_1) then
               Int32_Maps.Delete (Reference (DB, Socket), M.Arg_1);
            end if;
            Int32_Maps.Insert (Reference (DB, Socket), M.Arg_1, M.Arg_2);
         when 'Q' =>
            Response.Mean := Query (Reference (DB, Socket), M.Arg_1, M.Arg_2);
            Send_Socket (Socket, To_Bytes (Response), Last);
            if Last = 0 then
               Error := True;
               raise Program_Error with "Client disconnected before receiving response";
            end if;
         when others =>
            Error := True;
            raise Program_Error with "Invalid message type: " & M.Action;
      end case;
   exception
      when E : others =>
         Error := True;
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end On_Message;

   procedure On_Receive
      (Socket : GNAT.Sockets.Socket_Type;
       Item   : Ada.Streams.Stream_Element_Array;
       Last   : Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;
      use Stream_Buffers;

      MB : Message_Bytes;
      Error : Boolean;
      RX : Socket_Buffer_Maps.Reference_Type := Socket_Buffer_Maps.Reference (Buffers, Socket);
   begin
      if Last = 0 then
         Socket_Int32_Maps.Delete (DB, Socket);
      end if;

      for I in Item'First .. Last loop
         Append (RX, Item (I));
      end loop;

      while Length (RX) >= Message_Bytes'Length loop
         for I in MB'Range loop
            MB (I) := First_Element (RX);
            Delete_First (RX);
         end loop;
         On_Message (Socket, To_Message (MB), Error);
         if Error then
            GNAT.Sockets.Close_Socket (Socket);
            return;
         end if;
      end loop;
   exception
      when E : GNAT.Sockets.Socket_Error =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end On_Receive;

   procedure Run is
      EP : constant Epoll.Epoll_Descriptor := Epoll.Create;
   begin
      Stream_Buffers.Clear (Empty_Buffer);

      Mini.Bind (EP, "", "3000");
      loop
         Mini.Serve
            (EP         => EP,
             On_Connect => On_Connect'Access,
             On_Receive => On_Receive'Access,
             Max_Events => 32);
      end loop;
   end Run;

end Asset;
