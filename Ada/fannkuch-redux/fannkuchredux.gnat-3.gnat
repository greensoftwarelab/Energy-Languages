--
-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Based on code by Dave Fladebo, Eckehard Berns, Heiner Marxen, Hongwei Xi,
-- and The Anh Tran, and on the Java version of fannkuchredux by Oleg Mazurov.
-- Updated by Jonathan Parker and Georg Bauhaus, Nov 2012.
--

with Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;
with System;

procedure Fannkuchredux is

   Multitasking_Desired : constant Boolean := True;

   type Fann_Int is mod 2**System.Word_Size;

   pragma Assert (Ada.Command_Line.Argument_Count = 1,
     "Exactly one input argument is required.");

   N_image : constant String   := Ada.Command_Line.Argument (1);
   N       : constant Fann_Int := Fann_Int'Value (N_image);

   pragma Assert (N > 1,  "Input argument N must be integer > 1.");

   Fann_0     : constant Fann_Int := 0;
   Fann_First : constant Fann_Int := Fann_0;
   Fann_Last  : constant Fann_Int := Fann_0 + (N - 1);

   subtype Perm_Index is Fann_Int range Fann_First .. Fann_Last;

   type Permutation is array(Perm_Index) of Fann_Int;

   -- The N! permutations are indexed from 0 to N!-1.  The indices
   -- and the factorials have type Perm_id_Range.

   type Perm_id_Range is mod 2**System.Word_Size;
   pragma Assert (N < 13 or System.Word_Size = 64);
   pragma Assert (N < 21, "Input argument N must be integer < 21.");

   subtype Enum_Index is Fann_Int range Fann_First .. Fann_Last+1;
   type Enumeration is array(Enum_Index) of Perm_id_Range; -- holds N!'s

   No_of_Tasks : constant := 12;
   -- Using stnd setting of 12, Chunk_Size = (N! / No_of_Tasks) is even for N>3.

   type Task_id_Range is range 1 .. No_of_Tasks;

   type Checksum_Int is range
      -2**(System.Word_Size-1)+1 .. 2**(System.Word_Size-1)-1;


   procedure Swap (Perm1: in out Permutation; Hi, Lo: Fann_Int) is
      Tmp : constant Fann_Int := Perm1(Hi);
   begin
      Perm1(Hi) := Perm1(Lo);
      Perm1(Lo) := Tmp;
   end Swap;

   function Count_of_Flips
     (Perm : in Permutation)
      return Fann_Int
   is
      Lo_1st : constant Fann_Int := Fann_First + 1;
      Hi, Hi_1st, Tmp : Fann_Int;
      Flip_Count  : Fann_Int := 0;
      P_1st : Fann_Int;
      Perm1 : Permutation;
   begin
      P_1st := Perm(Perm'First);

      for i in Perm'Range loop
         Perm1(i) := Perm(i);
      end loop;

      loop                      -- Flip until P_1st = Fann_First

         exit when P_1st = Fann_First;
         Flip_Count := Flip_Count + 1;
         Hi_1st := P_1st - 1;

         if Lo_1st < Hi_1st then
            Hi := Hi_1st;
            for Lo in Lo_1st .. Lo_1st+16 loop
               Swap (Perm1, Hi, Lo);
               exit when Lo+3 > Hi;
               Hi := Hi - 1;
            end loop;
         end if;

         Tmp          := Perm1(P_1st);
         Perm1(P_1st) := P_1st;
         P_1st        := Tmp;

      end loop;

      return Flip_Count;

   end Count_of_Flips;

   procedure Get_First_Permutation
     (Perm_id   : in     Perm_id_Range;
      Factorial : in     Enumeration;
      Perm      :     out Permutation;
      Count     :     out Permutation)
   is
      d : Fann_Int;
      p_id : Perm_id_Range := Perm_id;
      Perm1 : Permutation;
   begin
      Perm  := (others => Fann_Int'First);
      Count := (others => Fann_Int'First);

      for i in Perm'Range loop
         Perm(i) := i;
      end loop;

      for i in reverse Fann_First+1 .. Fann_Last loop
         d        := Fann_Int (p_id  /  Factorial(i));
         p_id     := p_id mod Factorial(i);
         Count(i) := d;

         Perm1 := Perm;
         for j in Fann_First .. i loop
            if j+d <= i then
               Perm(j) :=  Perm1(j+d);
            else
               Perm(j) :=  Perm1(j+d-i-1);
            end if;
         end loop;
      end loop;

   end Get_First_Permutation;

   procedure Get_Next_Permutation
     (Perm  : in out Permutation;
      Count : in out Permutation)
   is
      Rotation_Upper_Bound : constant Fann_Int := 17;
      pragma Assert (Rotation_Upper_Bound >= Perm'Last);
      pragma Assert (Perm'First = 0);
      First, Next_First : Fann_Int;
      i : Fann_Int := 1;
   begin
      First    := Perm(1);
      Perm(1)  := Perm(0);
      Perm(0)  := First;
      Count(i) := Count(i) + 1;

      if Count(i) > i then
      loop
         exit when Count(i) <= i;
         Count(i) := 0;

         i := i + 1;

         Next_First := Perm(1);
         Perm(0)    := Next_First;
         for j in 2 .. Rotation_Upper_Bound loop
            Perm(j-1) := Perm(j);
            exit when j = i;
         end loop;
         Perm(i) := First;
         First   := Next_First;

         Count(i) := Count(i) + 1;

      end loop;
      end if;

   end Get_Next_Permutation;

   procedure Get_Checksum_and_Flips
     (Task_id   : in     Task_id_Range;
      Factorial : in     Enumeration;
      Max_Flips :    out Fann_Int;
      Checksum  :    out Checksum_Int)
   is
      Perm_id, Perm_id_Min, Perm_id_Max : Perm_id_Range;
      Flip_Count  : Fann_Int;
      Perm, Count : Permutation;
      Chunk_Size  : Perm_id_Range;
   begin

      Chunk_Size := Factorial(N) / No_of_Tasks;
      pragma Assert (Chunk_Size mod 2 = 0);
      --  so checksums work if No_of_Tasks>1.

      Perm_id_Min := Perm_id_Range (Task_id - Task_id_Range'First) * Chunk_Size;
      Perm_id_Max := Perm_id_Range'Min (Factorial(N), Perm_id_Min+Chunk_Size)-1;
      --  for the First task:   Perm_id_Min = 0;  Perm_id_Max := Chunk_Size-1
      --  Perm_id ultimately runs from 0 .. Factorial(N)-1

      Get_First_Permutation (Perm_id_Min, Factorial, Perm, Count);
      --  Initialize Perm and Count

      Max_Flips := 1;
      Checksum  := 0;
      Perm_id   := Perm_id_Min;
      loop

         if  Perm(0) > 0  then
            Flip_Count := Count_of_Flips (Perm);
            Max_Flips  := Fann_Int'Max (Max_Flips, Flip_Count);
            if Perm_id mod 2 = 0 then
               Checksum := Checksum + Checksum_Int (Flip_Count);
            else
               Checksum := Checksum - Checksum_Int (Flip_Count);
            end if;
         end if;

         exit when Perm_id >= Perm_id_Max;
         Perm_id := Perm_id + 1;
         Get_Next_Permutation (Perm, Count);

      end loop;

   end Get_Checksum_and_Flips;

   task type Flip_Counter is
      pragma Storage_Size (2**12);
      entry Start
        (Task_id   : in Task_id_Range;
         Factorial : in Enumeration);
      entry Return_Result
        (Partial_Flip_Count : out Fann_Int;
         Partial_Checksum   : out Checksum_Int);
   end Flip_Counter;


   task body Flip_Counter is
      Task_id_Local : Task_id_Range;
      Max_Flips     : Fann_Int;
      Checksum      : Checksum_Int;
      F : Enumeration;
   begin
      accept Start
        (Task_id   : in Task_id_Range;
         Factorial : in Enumeration)
      do
         Task_id_Local := Task_id;
         F := Factorial;
      end Start;

      Get_Checksum_and_Flips (Task_id_Local, F, Max_Flips, Checksum);

      accept Return_Result
        (Partial_Flip_Count : out Fann_Int;
         Partial_Checksum   : out Checksum_Int)
      do
         Partial_Flip_Count := Max_Flips;
         Partial_Checksum   := Checksum;
      end Return_Result;
   end Flip_Counter;

   type Flip_Data   is array (Task_id_Range) of Fann_Int;
   type Chksum_Data is array (Task_id_Range) of Checksum_Int;
   Flip_Count_Storage : Flip_Data   := (others => 0);
   Checksum_Storage   : Chksum_Data := (others => 0);
   Max_Flips : Fann_Int     := 0;
   Checksum  : Checksum_Int := 0;

   Factorial : Enumeration;

begin
   if not (N > 3 or (not Multitasking_Desired and No_of_Tasks = 1)) then
      Put_Line
        ("Set Multitasking_Desired = False and No_of_Tasks = 1 for N < 4");
      raise Program_Error;
   end if;

   Factorial(0) := 1;
   for i in Enum_Index range 1 .. Enum_Index'Last loop
      Factorial(i) := Factorial(i-1) * Perm_id_Range (i);
   end loop;

   if Multitasking_Desired then

      declare  -- and launch 1 task for each t in Task_id_Range:

         Counter : array(Task_id_Range) of Flip_Counter; -- the tasks.

      begin

         for t in Task_id_Range loop
            Counter(t).Start (t, Factorial);
         end loop;

         for t in Task_id_Range loop
            Counter(t).Return_Result (Max_Flips, Checksum);
            Flip_Count_Storage(t) := Max_Flips;
            Checksum_Storage(t)   := Checksum;
         end loop;

      end;

   else  -- Sequential:

      for t in Task_id_Range loop
         Get_Checksum_and_Flips (t, Factorial, Max_Flips, Checksum);
         Flip_Count_Storage(t) := Max_Flips;
         Checksum_Storage(t)   := Checksum;
      end loop;

   end if;

   Max_Flips := 0;
   for t in Task_id_Range loop
      if Flip_Count_Storage(t) > Max_Flips then
         Max_Flips := Flip_Count_Storage(t);
      end if;
   end loop;

   Checksum := 0;
   for t in Task_id_Range loop
      Checksum := Checksum + Checksum_Storage(t);
   end loop;

   declare
      C_Image : constant String := Checksum_Int'Image (Checksum);
   begin
      Put_Line (C_image(2..C_image'Last));
      Put ("Pfannkuchen("); Put (N_image); Put (") =");
      Put (Fann_Int'Image (Max_Flips));
   end;

end Fannkuchredux;
