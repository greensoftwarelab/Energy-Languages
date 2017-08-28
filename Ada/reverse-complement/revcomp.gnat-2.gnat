--
--  The Computer Language Benchmarks Game
--  http://benchmarksgame.alioth.debian.org/
--
--  Contributed by Pascal Obry on 2005/03/19
--  Modified by Bill Findlay  on 2005/04/04
--  Updated by Georg Bauhaus and Jonathan Parker (May 2012)

with Text_Input; use Text_Input;
with Line_IO;

procedure Revcomp is

   Multitasking_Version_Desired : constant Boolean := True;

   Complement : constant array (Character) of Character :=
     ('A' => 'T', 'C' => 'G', 'G' => 'C', 'T' => 'A', 'U' => 'A',
      'M' => 'K', 'R' => 'Y', 'W' => 'W', 'S' => 'S', 'Y' => 'R',
      'K' => 'M', 'V' => 'B', 'H' => 'D', 'D' => 'H', 'B' => 'V',
      'N' => 'N',
      'a' => 'T', 'c' => 'G', 'g' => 'C', 't' => 'A', 'u' => 'A',
      'm' => 'K', 'r' => 'Y', 'w' => 'W', 's' => 'S', 'y' => 'R',
      'k' => 'M', 'v' => 'B', 'h' => 'D', 'd' => 'H', 'b' => 'V',
      'n' => 'N',
      others => '?');

   Max_Line_Length : constant := 60;

   End_Of_Line : constant String := Line_IO.Separator;

   procedure Reverse_Fasta
     (Fasta_Line   : in     String_Access;
      Fasta_Start  : in     Natural;
      Fasta_Finish : in     Natural;
      Bundle       : in out String)
   is
      L : Natural := Bundle'First; -- Leftmost char
      R : Natural := Fasta_Finish; -- Rightmost char
      c0, c1 : Character;
   begin
      if R < Fasta_Start then return; end if;

      c1 := Fasta_Line(R);
      loop
         Bundle(L) := Complement(c1);
         R := R - 1;
         L := L + 1;
         if R > Fasta_Start then
            c0 := Fasta_Line(R);
            c1 := Fasta_Line(R-1);
            Bundle(L) := Complement(c0);
            L := L + 1;
            R := R - 1;
         else
            if R = Fasta_Start then
               Bundle(L) := Complement(Fasta_Line(R));
            end if;
            exit;
         end if;
      end loop;

   end Reverse_Fasta;

   procedure Put_Reversed_Fasta
     (Fasta_Section     : in String_Access;
      Fasta_Data_Length : in Natural)
   is
      Lines_per_Bundle : constant := 2000;
      Line_Feed_Len    : constant Natural := End_Of_Line'Length;
      Line_Bundle : String(1 .. Lines_per_Bundle*(Max_Line_Length + Line_Feed_Len));
      L        : Natural := Fasta_Data_Length;
      B_start  : Natural := Line_Bundle'First;
      B_finish : Natural := B_start + Max_Line_Length - 1;
   begin

      -- Append line feed string (End_Of_Line) to 2000 Line_Bundle lines:

      while L >= Lines_per_Bundle * Max_Line_Length loop
         B_start := Line_Bundle'First;
         for j in 1 .. Lines_per_Bundle loop
            B_finish := B_start + Max_Line_Length - 1;
            Reverse_Fasta
              (Fasta_Line   => Fasta_Section,
               Fasta_Start  => L - Max_Line_Length + 1,
               Fasta_Finish => L,
               Bundle       => Line_Bundle(B_start .. B_finish));
            Line_Bundle(B_finish + 1 .. B_finish + Line_Feed_Len) := End_Of_Line;
            B_start := B_finish + Line_Feed_Len + 1;
            L       := L - Max_Line_Length;
         end loop;
         Line_IO.Put (Line_Bundle);
      end loop;

      while L >= Max_Line_Length loop
         Reverse_Fasta
           (Fasta_Line   => Fasta_Section,
            Fasta_Start  => L - Max_Line_Length + 1,
            Fasta_Finish => L,
            Bundle       => Line_Bundle(1 .. Max_Line_Length));
         Line_IO.Put_Line (Line_Bundle (1..Max_Line_Length));
         L := L - Max_Line_Length;
      end loop;

      if L > 0 then
         Reverse_Fasta
           (Fasta_Line   => Fasta_Section,
            Fasta_Start  => 1,
            Fasta_Finish => L,
            Bundle       => Line_Bundle(1 .. L));
         Line_IO.Put_Line (Line_Bundle (1 .. L));
      end if;

   end Put_Reversed_Fasta;

   procedure Read_Reverse_Write_a_Section_p
     (Job_Is_Complete : out Boolean)
   is
      Section_o_Fasta : String_Pointer (2**20 * 128);
      Header          : String(1..Max_Line_Length) := (others => '?');
      Section_Length  : Natural := 0;
      Header_Length   : Natural := 0;
   begin
      Job_Is_Complete := False;

      Text_Input.Read_Section
        (Data_Buffer     => Section_o_Fasta.Buffer,
         Data_Length     => Section_Length,
         Next_Header     => Header,
         Header_Length   => Header_Length,
         Max_Line_Length => 100); -- use anything >= actual limit of 60.

      if Header_Length < 1 then   -- null Header marks final section.
         Job_Is_Complete := True;
      end if;

      if Section_Length > 0 then
         Put_Reversed_Fasta (Section_o_Fasta.Buffer, Section_Length);
      end if;
      if Header_Length > 0 then
         Line_IO.Put_Line (Header(1..Header_Length));
      end if;

   end Read_Reverse_Write_a_Section_p;

   task type Read_Reverse_Write_a_Section is
      entry Start_Reading;
      entry Done_Reading_Start_Writing (Reached_End_Of_File : out Boolean);
      entry Done_Writing;
      pragma Storage_Size (2**20);
   end Read_Reverse_Write_a_Section;

   task body Read_Reverse_Write_a_Section is
      Section_o_Fasta : String_Pointer (2**20 * 128);
      Header          : String(1..Max_Line_Length) := (others => '?');
      Section_Length  : Natural := 0;
      Header_Length   : Natural := 0;
      Hit_End_Of_File : Boolean := False;
   begin
      loop
      select
         accept Start_Reading;

         Text_Input.Read_Section
           (Data_Buffer     => Section_o_Fasta.Buffer,
            Data_Length     => Section_Length,
            Next_Header     => Header,
            Header_Length   => Header_Length,
            Max_Line_Length => 100); -- use anything >= actual limit of 60.

         if Header_Length < 1 then   -- null Header marks final section.
            Hit_End_Of_File := True;
         end if;

         accept Done_Reading_Start_Writing (Reached_End_Of_File : out Boolean) do
            Reached_End_Of_File := Hit_End_Of_File;
         end Done_Reading_Start_Writing;

         if Section_Length > 0 then
            Put_Reversed_Fasta (Section_o_Fasta.Buffer, Section_Length);
         end if;
         if Header_Length > 0 then
            Line_IO.Put_Line (Header(1..Header_Length));
         end if;

         accept Done_Writing;
      or
         terminate;
      end select;
      end loop;
   end Read_Reverse_Write_a_Section;

   Job_Is_Complete : Boolean;

begin

   if Multitasking_Version_Desired then -- Do computation concurrently with Input

      declare
         type Task_Id_Type is mod 2;
         Do_a_Section : array (Task_Id_Type) of Read_Reverse_Write_a_Section;
         i : Task_Id_Type := Task_Id_Type'First;
         Reached_End_Of_File : Boolean := False;
      begin

         Read_Reverse_Write_a_Section_p (Job_Is_Complete);
         --  All this does is handle the 1st line of the file (the Header).

         Do_a_Section(i).Start_Reading;
         --  Start 1st task reading 1st section.

         loop

            Do_a_Section(i).Done_Reading_Start_Writing (Reached_End_Of_File);
            -- Block here until task i says its done reading the section.
            -- After completion of this rendezvous, task i is unblocked. Task i
            -- then begins computing and writing the reversed data. Task i 
            -- remains unblocked until it finishes writing.

            -- Task i is done reading so we can unblock task i+1 to start reading:
            if not Reached_End_Of_File then
               Do_a_Section(i+1).Start_Reading;
            end if;

            Do_a_Section(i).Done_Writing;
            -- Block here until task i says it's done writing. (If task i+1 were
            -- to write while task i writes, then their output is interleaved.)
            -- Next go to top of loop to unblock task i+1 so that it can write.

            exit when Reached_End_Of_File;
            i := i + 1;

         end loop;

      end;

   else -- Use a Procedure rather than Tasks:

      loop
         Read_Reverse_Write_a_Section_p (Job_Is_Complete);
         exit when Job_Is_Complete;
      end loop;

   end if; -- Multitasking_Version_Desired

   Line_IO.Close;

end Revcomp;


with Ada.Unchecked_Deallocation;
with Ada.Finalization;

package Text_Input is

   -- Use Stream_IO to Read data from Standard_Input

   type String_Access is access String;
   type String_Pointer (Size : Positive) is new Ada.Finalization.Limited_Controlled with
       record
          Buffer : String_Access;
       end record;

   overriding procedure Initialize (Object : in out String_Pointer);
   overriding procedure Finalize (Object : in out String_Pointer);

   procedure Read_Section
     (Data_Buffer     : in out String_Access;
      Data_Length     :    out Natural;
      Next_Header     :    out String;
      Header_Length   :    out Natural;
      Max_Line_Length : in     Natural := 1024);

   Section_Marker : constant Character := '>';

   -- Read_Section reads until EOF or Section_Marker is found at start
   -- of a line. Can accept any line of length <= Max_Line_Length.

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

end Text_Input;

with Ada.IO_Exceptions;
with Line_IO;

package body Text_Input is

   procedure Read_Section
     (Data_Buffer     : in out String_Access;
      Data_Length     :    out Natural;
      Next_Header     :    out String;
      Header_Length   :    out Natural;
      Max_Line_Length : in     Natural := 1024)
   is
      Ptr : String_Access;
   begin
      Data_Length   := 0;
      Header_Length := 0;

      Fill_Data_Buffer:
      loop
         if Data_Length + Max_Line_Length > Data_Buffer'Length then
            Ptr := Data_Buffer;
            Data_Buffer := new String (1 .. 2 * Data_Buffer'Length);
            Data_Buffer (1 .. Data_Length) := Ptr (1 .. Data_Length);
            Free (Ptr);
         end if;

         Get_Next_Line:
         declare
            Line : constant String := Line_IO.Get_Line;
            Present_Line_Length : constant Natural := Line'Length;
            Strt : Natural;
         begin

            if Present_Line_Length < 1 then
               Header_Length := 0;
               exit Fill_Data_Buffer;
            end if;

            if Present_Line_Length > Max_Line_Length then
               raise Program_Error;
            end if;

            if Line(Line'First) = Section_Marker then
               Strt := Next_Header'First;
               Next_Header(Strt .. Strt + Present_Line_Length - 1) := Line;
               Header_Length := Present_Line_Length;
               exit Fill_Data_Buffer;
            else
               Data_Buffer(Data_Length+1 .. Data_Length+Present_Line_Length):=Line;
               Data_Length := Data_Length + Present_Line_Length;
            end if;

         end Get_Next_Line;

      end loop Fill_Data_Buffer;

   exception
      when Ada.IO_Exceptions.End_Error =>
        return;
   end Read_Section;

   overriding procedure Initialize (Object : in out String_Pointer) is
   begin
      Object.Buffer := new String (1 .. Object.Size);
   end Initialize;

   overriding procedure Finalize (Object : in out String_Pointer) is
   begin
      Free (Object.Buffer);
   end Finalize;

end Text_Input;

package Line_IO is

   --  Stream I/O of lines of text

   pragma Elaborate_Body (Line_IO);

   Separator : constant String := (1 => ASCII.LF);

   procedure Put_Line (Item : String);

   procedure Put (Item : String);

   function Get_Line return String;

   procedure Close;  --  close output

end Line_IO;


with Ada.Streams.Stream_IO;

package body Line_IO is

   use Ada.Streams;

   subtype Separator_Index is Stream_Element_Offset
       range 0 .. Separator'Length - 1;
   Separator_Bytes : constant Stream_Element_Array (Separator_Index) :=
       (0 => Character'Pos (Separator (1)));
   --  Converts Separator into type Stream_Element_Array. Used by Put_Line.

   Stdin  : Stream_IO.File_Type;
   Stdout : Stream_IO.File_Type;

   procedure Put_Line (Item : String) is
      subtype Index is Stream_Element_Offset range 1 .. Item'Length;
      subtype XBytes is Stream_Element_Array (Index);
      Item_Bytes: XBytes;
      pragma Import (Ada, Item_Bytes);
      for Item_Bytes'Address use Item'Address;
      pragma Assert (Item'Size = Item_Bytes'Size);
   begin
      Stream_IO.Write (Stdout, Item_Bytes);
      Stream_IO.Write (Stdout, Separator_Bytes);
   end Put_Line;

   procedure Put (Item : String) is
      subtype Index is Stream_Element_Offset range 1 .. Item'Length;
      subtype XBytes is Stream_Element_Array (Index);
      Item_Bytes: XBytes;
      pragma Import (Ada, Item_Bytes);
      for Item_Bytes'Address use Item'Address;
      pragma Assert (Item'Size = Item_Bytes'Size);
   begin
      Stream_IO.Write (Stdout, Item_Bytes);
   end Put;

   --  Declarations associated with filling a text buffer.

   BUFSIZ: constant := 8_192 * 8;
   pragma Assert(Character'Size = Stream_Element'Size);

   SL : constant Natural   := Separator'Length;

   subtype Extended_Buffer_Index is Positive range 1 .. BUFSIZ + SL;
   subtype Buffer_Index is Extended_Buffer_Index
     range Extended_Buffer_Index'First .. Extended_Buffer_Index'Last - SL;
   subtype Extended_Bytes_Index is Stream_Element_Offset
     range 1 .. Stream_Element_Offset(Extended_Buffer_Index'Last);
   subtype Bytes_Index is Extended_Bytes_Index
     range Extended_Bytes_Index'First
     .. (Extended_Bytes_Index'Last - Stream_Element_Offset(SL));

   subtype Buffer_Data is String(Extended_Buffer_Index);
   subtype Buffer_Bytes is Stream_Element_Array(Extended_Bytes_Index);

   Buffer : Buffer_Data;
   Bytes  : Buffer_Bytes;
   for Bytes'Address use Buffer'Address;
   pragma Import (Ada, Bytes);

   -- start of next substring and last valid character in buffer
   Position : Natural range 0 .. Extended_Buffer_Index'Last;
   Last     : Natural range 0 .. Buffer_Index'Last;
   End_Of_Input : Boolean;

   function Get_Line return String is

      procedure Reload is
         --  fill Buffer with bytes available
         Last_Filled : Stream_Element_Offset;
      begin
         if Last < Buffer_Index'Last then
            raise Stream_IO.End_Error;
         end if;
         Stream_IO.Read(Stdin,
           Item => Bytes(Bytes_Index),
           Last => Last_Filled);
         Last := Natural(Last_Filled);
         Position := 1;
         Buffer(Last + 1 .. Last + SL) := Separator;
      end Reload;

      function Separator_Position return Natural is
         --   index of next Separator_Sequence (may be sentinel)
         K : Extended_Buffer_Index := Position;
      begin
         loop
            if Buffer(K) = Separator(1) then
               exit;
            elsif Buffer(K+1) = Separator(1) then
               K := K + 1; exit;
            else
               K := K + 2;
            end if;
         end loop;
        return K;
      end Separator_Position;

      Next_Separator : Natural range 0 .. Extended_Buffer_Index'Last;
   begin  -- Get_Line

      if End_Of_Input then
         raise Stream_IO.End_Error;
      end if;

      Next_Separator := Separator_Position;

      if Next_Separator > Last then
         declare
            Result : constant String := Buffer(Position .. Last);
            subtype XString is String (1 .. Last - Position + 1);
         begin
            begin
               Reload;
               return XString(Result) & Get_Line;
            exception
               when Stream_IO.End_Error =>
                  End_Of_Input := True;
                  return XString(Result);
            end;
         end;
      else
         declare
            Result : String renames Buffer(Position .. Next_Separator - 1);
            subtype XString is String (1 .. Next_Separator - Position);
         begin
            Position := Next_Separator + SL;
            return XString (Result);
         end;
      end if;

      raise Program_Error;
   end Get_Line;

   procedure Close is
   begin
      Stream_IO.Close (Stdout);
   end Close;

begin
   Stream_IO.Open (Stdout,
      Mode => Stream_IO.Out_File,
      Name => "/dev/stdout");
   Stream_IO.Open (Stdin,
      Mode => Stream_IO.In_File,
      Name => "/dev/stdin");

   Buffer(Buffer_Index'Last + 1 .. Buffer'Last) := Separator;
   Position := Buffer_Index'Last + 1;
   Last     := Buffer_Index'Last;
   End_Of_Input := False;
end Line_IO;