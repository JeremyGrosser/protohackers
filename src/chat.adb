with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Maps;

with Epoll;
with Mini;

package body Chat is
   use Ada.Streams;

   type Session_State is (Initial, Login_Done);
   type Session is record
      State  : Session_State := Initial;
      Name   : Unbounded_String := To_Unbounded_String ("");
      Buffer : Unbounded_String := To_Unbounded_String ("");
   end record;

   Empty_Session : constant Session := (others => <>);

   function "<" (Left, Right : GNAT.Sockets.Socket_Type)
      return Boolean
   is (GNAT.Sockets.To_C (Left) < GNAT.Sockets.To_C (Right));

   package Socket_Session_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => GNAT.Sockets.Socket_Type,
       Element_Type  => Session);

   Sessions : Socket_Session_Maps.Map := Socket_Session_Maps.Empty_Map;

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
      use Socket_Session_Maps;
   begin
      Put ("Connection from ");
      Put (GNAT.Sockets.Image (Addr));
      New_Line;

      if not Contains (Sessions, Socket) then
         Insert (Sessions, Socket, Empty_Session);
      end if;
      Write (Socket, "Hello?" & ASCII.LF);
   end On_Connect;

   procedure Broadcast
      (Exclude : GNAT.Sockets.Socket_Type;
       Text    : String)
   is
      use Socket_Session_Maps;
      use GNAT.Sockets;
   begin
      for Cur in Iterate (Sessions) loop
         if Key (Cur) /= Exclude and then Element (Cur).State /= Initial then
            Write (Key (Cur), Text);
         end if;
      end loop;
   end Broadcast;

   function Name_List
      (Exclude : String)
      return String
   is
      package String_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Positive,
          Element_Type => Unbounded_String);
      use String_Vectors;
      use Socket_Session_Maps;
      Names : Vector := Empty_Vector;
      S : Unbounded_String;
      Last : Natural;
   begin
      for Cur in Iterate (Sessions) loop
         if Element (Cur).State /= Initial and then Element (Cur).Name /= Exclude then
            Append (Names, Element (Cur).Name);
         end if;
      end loop;

      Last := Natural (Length (Names));
      for I in 1 .. Last loop
         Append (S, Names (I));
         if I /= Last then
            Append (S, ", ");
         end if;
      end loop;

      return To_String (S);
   end Name_List;

   function Is_Valid_Name
      (Name : String)
      return Boolean
   is
      use Ada.Strings.Maps.Constants;
      use Ada.Strings.Maps;
   begin
      if Name'Length = 0 then
         return False;
      end if;

      for Ch of Name loop
         if not Is_In (Ch, Alphanumeric_Set) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Name;

   procedure On_Receive_Line
      (This   : in out Session;
       Socket : GNAT.Sockets.Socket_Type;
       Text   : String)
   is
   begin
      case This.State is
         when Initial =>
            if not Is_Valid_Name (Text) then
               Write (Socket, "Invalid name" & ASCII.LF);
               raise Constraint_Error with "Invalid name: " & Text;
            end if;
            This.Name := To_Unbounded_String (Text);
            Broadcast (Socket, "* " & Text & " has entered the room" & ASCII.LF);
            Write (Socket, "* The room contains: " & Name_List (Exclude => Text) & ASCII.LF);
            This.State := Login_Done;
         when Login_Done =>
            Broadcast (Socket, "[" & To_String (This.Name) & "] " & Text & ASCII.LF);
      end case;
   end On_Receive_Line;

   procedure On_Receive
      (Socket : GNAT.Sockets.Socket_Type;
       Item   : Ada.Streams.Stream_Element_Array;
       Last   : Ada.Streams.Stream_Element_Offset)
   is
      use Socket_Session_Maps;
      S     : constant Reference_Type := Reference (Sessions, Socket);
      Text  : constant String := To_String (Item (Item'First .. Last));
      EOL   : Natural;
   begin
      Put_Line (">>> " & Socket'Image & " " & Text);

      if Last = 0 then
         if Length (S.Buffer) > 0 then
            On_Receive_Line (S, Socket, To_String (S.Buffer));
         end if;

         if S.State = Login_Done then
            S.State := Initial;
            Broadcast (Socket, "* " & To_String (S.Name) & " has left the room" & ASCII.LF);
         else
            S.State := Initial;
         end if;

         Delete (S.Name, 1, Length (S.Name));
      else
         Append (S.Buffer, Text);
         loop
            EOL := Index (S.Buffer, "" & ASCII.LF);
            exit when EOL = 0;
            On_Receive_Line (S, Socket, Slice (S.Buffer, 1, EOL - 1));
            Delete (S.Buffer, 1, EOL);
         end loop;
      end if;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
         S := (others => <>);
         GNAT.Sockets.Close_Socket (Socket);
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
end Chat;
