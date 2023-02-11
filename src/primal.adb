with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with Epoll;
with Mini;

with JSON.Types;
with JSON.Parsers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Elementary_Functions;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;

package body Primal is

   subtype Scalar is Long_Long_Integer;

   type Any_Unbounded_String is access Unbounded_String;
   procedure Free is new Ada.Unchecked_Deallocation
      (Unbounded_String, Any_Unbounded_String);

   function "<" (Left, Right : GNAT.Sockets.Socket_Type)
      return Boolean
   is (GNAT.Sockets.To_C (Left) < GNAT.Sockets.To_C (Right));
   package Socket_Buffer_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => GNAT.Sockets.Socket_Type,
       Element_Type  => Any_Unbounded_String);
   use Socket_Buffer_Maps;

   Buffers : Socket_Buffer_Maps.Map := Empty_Map;

   package JSON_Types is new JSON.Types
      (Integer_Type           => Scalar,
       Float_Type             => Float,
       Maximum_Number_Length  => 64);
   package JSON_Parsers is new JSON.Parsers
      (Types                  => JSON_Types,
       Check_Duplicate_Keys   => True);

   procedure On_Connect
      (Socket : GNAT.Sockets.Socket_Type;
       Addr   : GNAT.Sockets.Sock_Addr_Type)
   is
      Buffer : Any_Unbounded_String;
   begin
      Put ("Connection from ");
      Put (GNAT.Sockets.Image (Addr));
      New_Line;
      if Contains (Buffers, Socket) then
         Buffer := Element (Buffers, Socket);
         Delete (Buffers, Socket);
         Free (Buffer);
      end if;
      Buffer := new Unbounded_String;
      Insert (Buffers, Socket, Buffer);
   end On_Connect;

   function Is_Prime
      (X : Scalar)
      return Boolean
   is
      use Ada.Numerics.Elementary_Functions;
   begin
      if X < 2 then
         return False;
      end if;

      for I in 2 .. Scalar (Sqrt (Float (X))) loop
         if X mod I = 0 then
            return False;
         end if;
      end loop;

      return True;
   end Is_Prime;

   procedure Write
      (Socket : GNAT.Sockets.Socket_Type;
       Text   : String)
   is
      use Ada.Streams;
      Item : Stream_Element_Array (1 .. Stream_Element_Offset (Text'Length))
         with Address => Text'Address;
      First : Stream_Element_Offset := Item'First;
      Last  : Stream_Element_Offset;
   begin
      loop
         GNAT.Sockets.Send_Socket (Socket, Item (First .. Item'Last), Last);
         exit when Last >= Item'Last;
         First := Last + 1;
      end loop;
   end Write;

   procedure Write_Line
      (Socket : GNAT.Sockets.Socket_Type;
       Text   : String)
   is
   begin
      Write (Socket, Text & ASCII.LF);
   end Write_Line;

   procedure On_Receive_Line
      (Socket : GNAT.Sockets.Socket_Type;
       Text   : String)
   is
      use JSON_Types;
      P : JSON_Parsers.Parser := JSON_Parsers.Create (Text);
      V : constant JSON_Value := P.Parse;

      Method : constant JSON_Value := V.Get ("method");
      Number : constant JSON_Value := V.Get ("number");

      M : constant String := Method.Value;
      N : Scalar;
      F : Float;
   begin
      if M = "isPrime" then
         case Number.Kind is
            when Integer_Kind =>
               N := Number.Value;
            when Float_Kind =>
               F := Number.Value;
               N := Scalar (F);
            when others =>
               raise Program_Error with "Unknown type for number object";
         end case;

         if Is_Prime (N) then
            Write_Line (Socket, "{""method"":""isPrime"",""prime"":true}");
         else
            Write_Line (Socket, "{""method"":""isPrime"",""prime"":false}");
         end if;
      else
         Write_Line (Socket, "{""error"":""Unknown method"",""method"":""" & M & """}");
      end if;
   end On_Receive_Line;

   procedure On_Receive
      (Socket : GNAT.Sockets.Socket_Type;
       Item   : Ada.Streams.Stream_Element_Array;
       Last   : Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      Text : String (Integer (Item'First) .. Integer (Last))
         with Import, Address => Item'Address;
      Buffer : Any_Unbounded_String := Element (Buffers, Socket);
      I : Integer;
   begin
      if Last = 0 then
         if Length (Buffer.all) > 0 then
            On_Receive_Line (Socket, To_String (Buffer.all));
         end if;

         Free (Buffer);
         Delete (Buffers, Socket);
      else
         Append (Buffer.all, Text);
         loop
            I := Index (Buffer.all, ASCII.LF & "");
            exit when I = 0;
            On_Receive_Line (Socket, Slice (Buffer.all, 1, I - 1));
            Delete (Buffer.all, 1, I);
         end loop;
      end if;
   exception
      when Constraint_Error =>
         --  fake it.
         if Index (Buffer.all, "bignumber") /= 0 then
            Write_Line (Socket, "{""method"":""isPrime"",""prime"":false}");
         else
            Write_Line (Socket, "{""method"":""error""}");
         end if;
      when E : others =>
         Write_Line (Socket, "{""method"":""error""}");
         Put_Line (">>> " & Text);
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end On_Receive;

   procedure Run is
      EP : constant Epoll.Epoll_Descriptor := Mini.Bind ("", "3000");
   begin
      loop
         Mini.Serve
            (EP         => EP,
             On_Connect => On_Connect'Access,
             On_Receive => On_Receive'Unrestricted_Access,
             Max_Events => 64);
      end loop;
   end Run;

end Primal;
