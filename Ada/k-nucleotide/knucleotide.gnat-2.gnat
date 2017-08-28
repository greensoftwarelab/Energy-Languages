--  The Computer Language Benchmarks Game
--  http://benchmarksgame.alioth.debian.org/
--
--  Contributed by Martin Krischik
--  Modified by Georg Bauhaus and Jonathan Parker

pragma Restrictions (No_Abort_Statements);
pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);

with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Text_IO;
with Ada.Exceptions;

with String_Fragments;
with Data_Input;
with GNAT.Heap_Sort_G;
with GNAT.HTable;

procedure KNucleotide is

   subtype Fragment_Lengths is Integer range 1 .. 18;

   --  Data read as single String:

   Buffer : constant String := Data_Input.Read;

   --  Calculate and write data - either a percentage for all fragments found
   --  or - when Nucleotide_Fragment is given - the count for that fragment.

   generic
      with package Fragments is new String_Fragments(<>);
   package Work is

      procedure Write
        (Nucleotide_Length   : in Fragment_Lengths;
         Nucleotide_Fragment : in Fragments.Fragment := Fragments.Null_Fragment);

      task Writer is
         --
         --  Performs `Write` calls
         --
         pragma Storage_Size (2**24);

         entry Set (Nucleotide_Length   : in Fragment_Lengths;
                    Nucleotide_Fragment : in Fragments.Fragment := Fragments.Null_Fragment);
      end Writer;

   end Work;


   type Order is array (Fragment_Lengths) of Boolean;

   protected Printer is
      --
      --  Serializes access to output
      --
      entry Seize (Fragment_Lengths);
      procedure Release (L : Fragment_Lengths);

   private
      Done : Order :=
        (1 | 2 | 3 | 4 | 6 | 12 | 18 => False,
         others => True);
   end Printer;

   protected body Printer is

      --  Use families' entry indexes to find that output of preceding
      --  `Write`s has already been produced thus ordering the tasks's results.

      entry Seize (for L in Fragment_Lengths)
      when Done (1 .. Fragment_Lengths'Pred(L)) = (1 .. Fragment_Lengths'Pred(L) => True) is
      begin
         null;
      end Seize;

      procedure Release (L : Fragment_Lengths) is
      begin
         Done (L) := True;
      end Release;

   end Printer;

   package body Work is

      ---------------------
      -- procedure Write --
      ---------------------

      --  Procedure KNucleotide's tasks call Write in order to calculate and
      --  write data - either a percentage for all fragments found or - when
      --  Nucleotide_Fragment is given - the count for that fragment.
      --
      procedure Write
        (Nucleotide_Length   : in Fragment_Lengths;
         Nucleotide_Fragment : in Fragments.Fragment := Fragments.Null_Fragment)
      is
         use Fragments;

         --  Package is an interface to GNAT's simple hash table: GNAT.HTable.
         --  The package calculates nucleotide Fragment_Lengths and keeps the
         --  result inside a hash table as requested by the shootout rules.

         package Calculator is

            --  Elements used to store inside hash table:

            type Element_Type is private;
            type Element_Access is access Element_Type;
            for Element_Access'Storage_Size use 16#60_00_01#;


--  Calculate frequency of occurrence of the nucleotides:

            procedure Get_Frequencies (Length : Fragment_Lengths);

            --  Get the count for the given nucleotide fragment:

            function Number_Counted (Nucleotide_Fragment : Fragment) return Natural;

            --  Start to iterate over all elements of hash table:

            function Get_First return Element_Access;

            --  Continue itereation over the hash table:

            function Get_Next return Element_Access;

            --  Key and value when computed:

            function Count_Of (Element : not null Element_Access) return Natural;
            function Fragment_Of (Element : not null Element_Access) return Fragment;

            --  Get total count over all elements - as well as the count of
            --  elements:

            procedure Get_Totals (Total : out Natural; Count : out Natural);

         private
            pragma Inline (Count_of, Fragment_Of);

            type Element_Type is record
               Count : Natural        := 0;
               Key   : Fragment       := Fragments.Null_Fragment;
               Next  : Element_Access := null;
            end record;
         end Calculator;

         package body Calculator is

            Log_Table_Size : constant Natural := Natural'Min (Fragment'Last*2+4, 17);
            Table_Size     : constant Natural := 2 ** Log_Table_Size;

            subtype Hash_Type is Natural range 0 .. Table_Size - 1;

            function Hash (Key : Fragment) return Hash_Type;
            procedure Set_Next (E : Element_Access; Next : Element_Access);
            function Next (E : Element_Access) return Element_Access;
            function Get_Key (E : not null Element_Access) return Fragment;

            pragma Inline (Hash, Set_Next, Next, Get_Key);

            package Table is new GNAT.HTable.Static_HTable
              (Header_Num => Hash_Type,
               Element    => Element_Type,
               Elmt_Ptr   => Element_Access,
               Null_Ptr   => null,
               Key        => Fragment,
               Hash       => Hash,
               Equal      => Fragments."=",
               Set_Next   => Set_Next,
               Next       => Next,
               Get_Key    => Get_Key);


            function Hash (Key : Fragment) return Hash_Type is
               pragma Assert (Hash_Type'First = 0);
               pragma Assert (Hash_Type'Last  = 2**Log_Table_Size - 1);
               type Uns_32 is mod 2**32;
               H : Uns_32 := Character'Pos (Key (Key'First));
            begin
               for J in Key'First + 1 .. Key'Last loop
                  H := Character'Pos (Key (J)) + H * 2**3 + H;
               end loop;
               H := (H / 2**Log_Table_Size) xor H;
               return Hash_Type'Base (H mod 2**Log_Table_Size);
            end Hash;


            procedure Get_Frequencies (Length : Fragment_Lengths) is
            begin
               for I in  1 .. Buffer'Last - Length + 1 loop
                  declare
                     Key : String renames Buffer(I .. I + Length - 1);
                     Element : constant Element_Access := Table.Get (Key);
                  begin
                     if Element /= null then
                        Element.all.Count := Natural'Succ (Element.all.Count);
                     else
                        Table.Set (new Element_Type'(Count => 1,
                                                     Key => Key,
                                                     Next => null));
                     end if;
                  end;
               end loop;
               return;
            end Get_Frequencies;


            function Count_Of (Element : not null Element_Access) return Natural is
            begin
               return Element.all.Count;
            end Count_Of;


            function Number_Counted (Nucleotide_Fragment : Fragment) return Natural is
               The_Element : constant Element_Access := Table.Get (Nucleotide_Fragment);
            begin
               if The_Element /= null then
                  return The_Element.all.Count;
               else
                  return 0;
               end if;
            end Number_Counted;


            function Get_First return Element_Access is
            begin
               return Table.Get_First;
            end Get_First;


            function Get_Key (E : not null Element_Access) return Fragment is
            begin
               return E.all.Key;
            end Get_Key;


            function Get_Next return Element_Access is
            begin
               return Table.Get_Next;
            end Get_Next;


            procedure Get_Totals (Total : out Natural; Count : out Natural) is
               The_Element : Element_Access := Table.Get_First;
            begin
               Total := 0;
               Count := 0;
               while The_Element /= null loop
                  Total       := Total + The_Element.all.Count;
                  Count       := Count + 1;
                  The_Element := Table.Get_Next;
               end loop;
            end Get_Totals;


            function Fragment_Of (Element : not null Element_Access) return Fragment is
            begin
               return Element.all.Key;
            end Fragment_Of;

            function Next (E : Element_Access) return Element_Access is
            begin
               return E.all.Next;
            end Next;

            procedure Set_Next (E : Element_Access; Next : Element_Access) is
            begin
               E.all.Next := Next;
            end Set_Next;

         end Calculator;

      begin --  Write

         Calculator.Get_Frequencies (Nucleotide_Length);

         if Nucleotide_Fragment = Fragments.Null_Fragment then
            Calculate_Total : declare
               Num_Table_Entries : Natural;
               Sum_Of_Counts     : Natural;
            begin
               Calculator.Get_Totals
                 (Total => Sum_Of_Counts,
                  Count => Num_Table_Entries);

               Get_Sort_Put : declare
                  Data : array (0 .. Num_Table_Entries) of Calculator.Element_Access;

                  --  heap sort subprograms
                  procedure Move (From : Natural; To : Natural);
                  function Less_Then (Op1, Op2 : Natural) return Boolean;

                  pragma Inline (Move, Less_Then);

                  function Less_Then (Op1, Op2 : Natural) return Boolean is
                  begin
                     return
                       Calculator.Count_Of (Data (Op1))
                       >
                       Calculator.Count_Of (Data (Op2));
                  end Less_Then;

                  procedure Move (From : Natural; To : Natural) is
                  begin
                     Data (To) := Data (From);
                  end Move;

                  package Heap_Sort is new GNAT.Heap_Sort_G
                    (Move => Move,
                     Lt   => Less_Then);

               begin  -- Get_Sort_Put
                  Data (0) := null;
                  Data (1) := Calculator.Get_First;

                  for I in  2 .. Data'Last loop
                     Data (I) := Calculator.Get_Next;
                  end loop;

                  Heap_Sort.Sort (Data'Last);

                  Printer.Seize (Nucleotide_Length);
                  for I in  1 .. Data'Last loop
                     Ada.Text_IO.Put (Calculator.Fragment_Of (Data (I)) & ' ');
                     Ada.Float_Text_IO.Put
                       (Item => (100.0
                                   * Float (Calculator.Count_Of (Data (I)))
                                   / Float (Sum_Of_Counts)),
                        Fore => 1,
                        Aft  => 3,
                        Exp  => 0);
                     Ada.Text_IO.New_Line;
                  end loop;
                  Ada.Text_IO.New_Line;
                  Printer.Release (Nucleotide_Length);
               end Get_Sort_Put;
            end Calculate_Total;
         else
            Printer.Seize (Nucleotide_Length);
            Ada.Integer_Text_IO.Put
              (Item => Calculator.Number_Counted (Nucleotide_Fragment),
               Width => 1);
            Ada.Text_IO.Put (Ada.Characters.Latin_1.HT);
            Ada.Text_IO.Put_Line (Nucleotide_Fragment);
            Printer.Release (Nucleotide_Length);
         end if;
      end Write;

      task body Writer is
         Current_Length   : Fragment_Lengths;
         Current_Fragment : Fragments.Fragment;

         use Fragments;
      begin
         loop
            --
            --  perform a `Write` with parameters `Set` or terminate
            --
            select
               accept Set (Nucleotide_Length   : in Fragment_Lengths;
                           Nucleotide_Fragment : in Fragment := Null_Fragment)
               do
                  Current_Length   := Nucleotide_Length;
                  Current_Fragment := Nucleotide_Fragment;
               end Set;
               Write (Current_Length, Current_Fragment);
            or
               terminate;
            end select;
         end loop;
      exception
         when E : others =>
            Ada.Text_IO.Put_Line(Ada.Text_IO.Current_Error,
              Ada.Exceptions.Exception_Information(E));
      end Writer;

   end Work;


   package Fragments_1 is new String_Fragments (1);
   package Fragments_2 is new String_Fragments (2);
   package Fragments_3 is new String_Fragments (3);
   package Fragments_4 is new String_Fragments (4);
   package Fragments_6 is new String_Fragments (6);
   package Fragments_12 is new String_Fragments (12);
   package Fragments_18 is new String_Fragments (18);

   --  List of fragments to be analyzed for this test:

   Fragment_3  : constant Fragments_3.Fragment := Fragments_3.To_Fragment ("GGT");
   Fragment_4  : constant Fragments_4.Fragment := Fragments_4.To_Fragment ("GGTA");
   Fragment_6  : constant Fragments_6.Fragment := Fragments_6.To_Fragment ("GGTATT");
   Fragment_12 : constant Fragments_12.Fragment := Fragments_12.To_Fragment ("GGTATTTTAATT");
   Fragment_18 : constant Fragments_18.Fragment := Fragments_18.To_Fragment ("GGTATTTTAATTTATAGT");

   package Work_On_1 is new Work (Fragments_1);
   package Work_On_2 is new Work (Fragments_2);
   package Work_On_3 is new Work (Fragments_3);
   package Work_On_4 is new Work (Fragments_4);
   package Work_On_6 is new Work (Fragments_6);
   package Work_On_12 is new Work (Fragments_12);
   package Work_On_18 is new Work (Fragments_18);

begin
   Work_On_1.Writer.Set (1);
   Work_On_12.Writer.Set (Fragment_12'Length, Fragment_12);
   Work_On_18.Writer.Set (Fragment_18'Length, Fragment_18);
   Work_On_6.Writer.Set (Fragment_6'Length, Fragment_6);
   Work_On_2.Writer.Set (2);
   Work_On_4.Writer.Set (Fragment_4'Length, Fragment_4);
   Work_On_3.Writer.Set (Fragment_3'Length, Fragment_3);
end KNucleotide;



generic
   Max_String_Length : Positive;
package String_Fragments is

   subtype Fragment is String (1 .. Max_String_Length);

   function To_Fragment (Source : String) return Fragment;
   function Null_Fragment return Fragment;
   function "=" (Left, Right: Fragment) return Boolean;

end String_Fragments;


with Ada.Unchecked_Conversion;

package body String_Fragments is

   Bytes_Per_Word : constant := 4;
   type Uns is mod 2**(8 * Bytes_Per_Word);
   for Uns'Size use 8 * Bytes_Per_Word;
   subtype Str is String (1 .. Bytes_Per_Word);

   function Null_Fragment return Fragment is
   begin
      return Fragment'(1 .. Max_String_Length => '*');
   end Null_Fragment;


   function To_Uns is new Ada.Unchecked_Conversion (Str, Uns);

   function "=" (Left, Right: Fragment) return Boolean is
      Strt : Integer := 1;
      Fnsh : Integer := Bytes_Per_Word;
      Last : constant Integer := Left'Last;
   begin
      if Last /= Right'Last then
         return False;
      end if;

      loop
         exit when Fnsh > Last;
         if To_Uns (Left(Strt..Fnsh)) /= To_Uns (Right(Strt..Fnsh)) then
            return False;
         end if;
         Strt := Strt + Bytes_Per_Word;
         Fnsh := Fnsh + Bytes_Per_Word;
      end loop;

      for I in Strt .. Last loop
         if Left(I) /= Right(I) then
            return False;
         end if;
      end loop;
      return True;
   end "=";


   function To_Fragment (Source : String) return Fragment is
      Result : Fragment;
   begin
      if Source'Length /= Max_String_Length then
         raise Constraint_Error;
      end if;
      Result (1 .. Source'Length) := Source;
      return Result;
   end To_Fragment;

end String_Fragments;


----------------
-- data input --
----------------

package Data_Input is

   --  Read data from Standard_Input and return section THREE as String:

   function Read return String;

end Data_Input;

with Ada.Strings.Maps.Constants;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Line_IO;
with Ada.Unchecked_Deallocation;

package body Data_Input is

   use Ada.Strings;
   UnixLF : constant String := String'(1 => ASCII.LF);
   package LIO is new Line_IO (UnixLF);

   Data_Buffer : Unbounded.Unbounded_String := Unbounded.Null_Unbounded_String;

   Section_Marker : constant Character := '>';
   Section        : constant String    := Section_Marker & "THREE";

   --  Read next data section - until EOF oder a line beginning with > is found.

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Read_Section is
      Buffer     : String_Access;
      Read_First : Natural;
      Read_Last  : Natural;
   begin
      Buffer := new String (1 .. 1024 * 1024 * 16);
      Get_Data : loop
         Read_First := Buffer'First;
         Read_Last  := Buffer'First - 1;
         -- fill Buffer and append to Data_Buffer when filled
         loop
            declare
               Line : String renames LIO.Get_Line;
            begin
               Read_Last := Read_First + Line'Length - 1;
               if Read_Last >= Buffer'Last then
                  Unbounded.Append
                    (Data_Buffer, New_Item => Buffer(1 .. Read_First - 1));
                  Unbounded.Append (Data_Buffer, New_Item => Line);
                  exit;
               end if;
               Buffer (Read_First .. Read_Last) := Line;
            end;
            exit Get_Data when Buffer (Read_First) = Section_Marker;
            Read_First := Read_Last + 1;
         end loop;
      end loop Get_Data;
      Unbounded.Append (Data_Buffer, Buffer (1 .. Read_Last));
      Free (Buffer);
   exception
      when Ada.IO_Exceptions.End_Error =>
         Unbounded.Append (Data_Buffer, Buffer (1 .. Read_Last));
         Free (Buffer);
   end Read_Section;


   --  Skip data on Standard_Input until ">THREE" is found

   procedure Skip_To_Section is
   begin
      loop
         declare
            Line : constant String := LIO.Get_Line;
         begin
            exit when Line(1) = Section(1)
              and then Line(Section'Range) = Section;
         end;
      end loop;
   end Skip_To_Section;


   function Read return String is
   begin
      Skip_To_Section;
      Read_Section;

      Unbounded.Translate
        (Source => Data_Buffer,
         Mapping => Maps.Constants.Upper_Case_Map);

      return Unbounded.To_String (Data_Buffer);
   end Read;

end Data_Input;

---------------------------
--  Stream I/O of lines --
---------------------------
generic
   Separator_Sequence : in String;  --  ends a line
package Line_IO is

   pragma Elaborate_Body;

   procedure Put_Line (Item : String) is null;
   -- not used in this program

   function Get_Line return String;

end Line_IO;


with Ada.Streams.Stream_IO;

package body Line_IO is

   use Ada.Streams;

   Stdin : Stream_IO.File_Type;

   -- Types etc., status variables, and the buffer.

   BUFSIZ: constant := 8_192;
   pragma Assert(Character'Size = Stream_Element'Size);

   SL : constant Natural := Separator_Sequence'Length;

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
         Buffer(Last + 1 .. Last + SL) := Separator_Sequence;
      end Reload;


      function Separator_Position return Natural is
         --   index of next Separator_Sequence (may be sentinel)
         pragma Inline(Separator_Position);
         K : Extended_Buffer_Index := Position;
      begin
         while Buffer(K) /= Separator_Sequence(1) loop
            K := K + 1;
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


begin
   Stream_IO.Open (Stdin,
     Mode => Stream_IO.In_File,
     Name => "/dev/stdin");

   Buffer(Buffer_Index'Last + 1 .. Buffer'Last) := Separator_Sequence;
   Position := Buffer_Index'Last + 1;
   Last := Buffer_Index'Last;
   End_Of_Input := False;
end Line_IO;