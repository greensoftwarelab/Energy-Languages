--  The Computer Language Benchmarks Game
--  http://benchmarksgame.alioth.debian.org
--
--  Contributed by Jim Rogers
--  Modified by Pascal Obry, Gautier de Montmollin, Georg Bauhaus, Jonathan Parker

pragma Restrictions (No_Abort_Statements);
pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);

with Ada.Command_Line;      use Ada.Command_Line;

with Interfaces;            use Interfaces;
with Ada.Streams.Stream_IO; use Ada.Streams;

procedure Mandelbrot is

   type Real is digits 15;
   type M8 is mod 8;
   type PCount is range 0 .. 2**20 - 1;

   Iter        : constant := 50;
   Limit       : constant := 4.0;
   Size        : constant Positive := Positive'Value (Argument (1));
   Two_on_Size : constant Real := 2.0 / Real (Size);

   subtype Output_Index is Stream_Element_Offset range 1 .. Stream_Element_Offset'Last;
   subtype Output_Queue is Stream_Element_Array;
   type Output is access Output_Queue;

   task type X_Step is
      entry Compute_Z (Y1, Y2 : PCount);
      entry Get_Output (Result : out Output);
   end X_Step;

   procedure Allocate_Output_Queue (Y1, Y2 : PCount; Result : out Output);
   pragma Precondition
     (Output_Index'First = 1);
   pragma Postcondition
     (Result'First = Output_Index'First and
      Result'Last = Output_Index'Max
        (0, Stream_Element_Offset (Integer (Y2 - Y1) * (Size / 8 + Boolean'Pos (Size mod 8 > 0)))));

   procedure Compute (Y1, Y2 : PCount; Result : Output)
   is
      subtype Instruction_Stream_Index is Integer range 1 .. 2;
      pragma Assert (Instruction_Stream_Index'First = 1);

      Bit_Num  : M8         := 0;
      Byte_Acc : Unsigned_8 := 0;
      Byte_Acc_Storage : array (Instruction_Stream_Index) of Unsigned_8;
      Last     : Stream_Element_Offset := Result'First - 1;
   begin
      for Y in Y1 .. Y2 - 1 loop
         for X in 0 .. Size / Instruction_Stream_Index'Last - 1 loop
            declare
               Cr_1 : constant Real := Two_on_Size * (Real (2*X)) - 1.5;
               Ci_1 : constant Real := Two_on_Size * (Real (Y)) - 1.0;
               Zr_1 : Real := Cr_1;
               Zi_1 : Real := Ci_1;
               ZZi_1 : Real := Zi_1 * Zi_1;
               ZZr_1 : Real := Zr_1 * Zr_1;
               Z_1_Exceeded_Limit : Boolean := False;
               Tmp_1 : Real;

               Cr_2 : constant Real := Two_on_Size * (Real (2*X + 1)) - 1.5;
               Ci_2 : constant Real := Two_on_Size * (Real (Y)) - 1.0;
               Zr_2 : Real := Cr_2;
               Zi_2 : Real := Ci_2;
               ZZi_2 : Real := Zi_2 * Zi_2;
               ZZr_2 : Real := Zr_2 * Zr_2;
               Z_2_Exceeded_Limit : Boolean := False;
               Tmp_2 : Real;
            begin
               for I in 1 .. Iter loop

                  Tmp_1 := Zr_1 * Zi_1;
                  Tmp_2 := Zr_2 * Zi_2;
                  Zr_1 := Cr_1 - ZZi_1;
                  Zr_2 := Cr_2 - ZZi_2;
                  Zi_1 := Ci_1 + Tmp_1 + Tmp_1;
                  Zi_2 := Ci_2 + Tmp_2 + Tmp_2;
                  Zr_1 := Zr_1 + ZZr_1;
                  Zr_2 := Zr_2 + ZZr_2;

                  if not Z_1_Exceeded_Limit then
                     if ZZi_1 + ZZr_1 > Limit then
                        Z_1_Exceeded_Limit := True;
                     end if;
                  end if;
                  if not Z_1_Exceeded_Limit then
                     ZZr_1 := Zr_1 * Zr_1;
                     ZZi_1 := Zi_1 * Zi_1;
                  end if;

                  if not Z_2_Exceeded_Limit then
                     if ZZi_2 + ZZr_2 > Limit then
                        Z_2_Exceeded_Limit := True;
                     end if;
                  end if;
                  if not Z_2_Exceeded_Limit then
                     ZZr_2 := Zr_2 * Zr_2;
                     ZZi_2 := Zi_2 * Zi_2;
                  end if;

                  exit when Z_2_Exceeded_Limit and Z_1_Exceeded_Limit;
               end loop;

               if Z_1_Exceeded_Limit then
                  Byte_Acc := Shift_Left (Byte_Acc, 1) or 16#00#;
               else
                  Byte_Acc := Shift_Left (Byte_Acc, 1) or 16#01#;
               end if;
               Byte_Acc_Storage (1) := Byte_Acc;

               if Z_2_Exceeded_Limit then
                  Byte_Acc := Shift_Left (Byte_Acc, 1) or 16#00#;
               else
                  Byte_Acc := Shift_Left (Byte_Acc, 1) or 16#01#;
               end if;
               Byte_Acc_Storage (2) := Byte_Acc;
            end;

            for j in Byte_Acc_Storage'Range loop

               Bit_Num := Bit_Num + 1;

               if Bit_Num = 0 then
                  Last := Last + 1;
                  Result (Last) := Stream_Element (Byte_Acc_Storage (j));
                  Byte_Acc := 0;
               end if;

            end loop;
         end loop;

         case M8 (Size mod 8) is
            when 0 =>
               null;
            when 1 | 3 | 5 | 7 =>
               pragma Assert (False);  -- odd Size not covered
               null;
            when 2 | 4 | 6 =>
               Byte_Acc_Storage (2) := Shift_Left (Byte_Acc, 8 - (Size mod 8));
               Last := Last + 1;
               Result (Last) := Stream_Element (Byte_Acc_Storage (2));
               Byte_Acc := 0;
               Bit_Num  := 0;
         end case;
      end loop;
   end Compute;

   task body X_Step is
      Y1, Y2 : PCount;
      Data   : Output;
   begin
      accept Compute_Z (Y1, Y2 : PCount) do
         X_Step.Y1 := Y1;
         X_Step.Y2 := Y2;
      end Compute_Z;

      Allocate_Output_Queue (Y1, Y2, Result => Data);
      Compute (Y1, Y2, Result => Data);

      accept Get_Output (Result : out Output) do
         Result := Data;
      end Get_Output;
   end X_Step;

   procedure Allocate_Output_Queue (Y1, Y2 : PCount; Result : out Output) is
      Limit : constant Natural := Natural'Max
        (0,
         Integer (Y2 - Y1) * (Size / 8 + Boolean'Pos (Size mod 8 > 0)));
   begin
      Result := new Output_Queue (1 .. Output_Index'Base (Limit));
   end Allocate_Output_Queue;

begin
   pragma Assert (Size mod 2 = 0, "Size must be even at present");

   declare
      subtype Worker_Index is Natural range 0 .. 32;
      Chunk_Size : constant Positive :=
        (Size + Worker_Index'Last) / Worker_Index'Last;
      Worker     : array (Worker_Index) of X_Step;
      pragma       Assert (Worker'Length * Chunk_Size >= Size);
      pragma       Assert (Worker'First = 0);
   begin
      for P in Worker'Range loop
         Worker (P).Compute_Z
           (Y1 => PCount (P * Chunk_Size),
            Y2 => PCount (Positive'Min ((P + 1) * Chunk_Size, Size)));
      end loop;

      declare
         Stdout : Stream_IO.File_Type;
         Header : constant String := "P4" & ASCII.LF &
           Argument (1) & " " & Argument (1) & ASCII.LF;
         Buffer : Output;

         Header_Bytes : Stream_Element_Array (1 .. Header'Length);
         pragma Import (Ada, Header_Bytes);
         for Header_Bytes'Address use Header'Address;
      begin
         Stream_IO.Open (Stdout, Stream_IO.Out_File, "/dev/stdout");
         Stream_IO.Write (Stdout, Header_Bytes);
         for P in Worker'Range loop
            Worker (P).Get_Output (Result => Buffer);
            Stream_IO.Write (Stdout, Buffer.all);
         end loop;
      end;
   end;

end Mandelbrot;
