-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org
--
-- contributed by Pascal Obry on 2005/04/07
-- modified by Gautier de Montmollin
-- modified by Georg Bauhaus, Jonathan Parker (July 2011)

with Ada.Command_Line;
with GNAT.Float_Control;
with Sequence.Data, Sequence.Creation;

procedure Fasta is

   N : constant Positive := Positive'Value (Ada.Command_Line.Argument (1));
   
   use Sequence.Data, Sequence.Creation;
   
   Runner : Environment;
begin
   GNAT.Float_Control.Reset;

   Make_Repeat_Fasta (">ONE Homo sapiens alu", ALU, N*2);
   Make_Random_Fasta (">TWO IUB ambiguity codes", IUB, N*3);
   Make_Random_Fasta (">THREE Homo sapiens frequency", Homosapiens, N*5);

end Fasta;
    
package Sequence.Data is
   
   pragma Pure (Data);
   
   Homosapiens : constant Nucleotide_Set(0..3) :=
    (('a', 0.3029549426680), ('c', 0.1979883004921),
     ('g', 0.1975473066391), ('t', 0.3015094502008));

   IUB : constant Nucleotide_Set(0..14) :=
    (('a', 0.27), ('c', 0.12), ('g', 0.12), ('t', 0.27),
     ('B', 0.02), ('D', 0.02), ('H', 0.02), ('K', 0.02),
     ('M', 0.02), ('N', 0.02), ('R', 0.02), ('S', 0.02),
     ('V', 0.02), ('W', 0.02), ('Y', 0.02));

   ALU : constant String :=
     "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" &
     "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" &
     "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" &
     "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" &
     "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" &
     "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" &
     "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";
   
end Sequence.Data;


with Ada.Finalization;

package Sequence.Creation is

   procedure Make_Random_Fasta
     (Title       : in String; 
      Nucleotides : in Nucleotide_Set; 
      N           : in Positive);

   procedure Make_Repeat_Fasta 
     (Title : in String;
      S     : in String; 
      N     : in Positive);
   
   type Environment is new Ada.Finalization.Limited_Controlled
     with null record;

private

   overriding
   procedure Initialize (Active : in out Environment);

   overriding
   procedure Finalize (Active : in out Environment);

end Sequence.Creation;

package Sequence is
   
   pragma Pure (Sequence);
   
   type Real is digits 15;

   type Nucleotide is record
      C : Character;
      P : Real;
   end record;

   Max_Length_of_Code : constant := 15;

   subtype Nucleotide_Index is Integer range 0 .. Max_Length_of_Code-1;

   type Nucleotide_Set is array (Nucleotide_Index range <>) of Nucleotide;

end Sequence;

with LCG_Random;
with Line_IO;

package body Sequence.Creation is

   package Real_Random_Nums is new LCG_Random (Real);
   use Real_Random_Nums;
   
   overriding
   procedure Initialize (Active : in out Environment) is
   begin
      Line_IO.Open_Stdout;
   end Initialize;
   
   overriding
   procedure Finalize (Active : in out Environment) is
   begin
      Line_IO.Close_Stdout;
   end Finalize;


   Line_Length : constant := 60;
   End_of_Line : String renames Line_IO.End_of_Line;

   subtype Line_End_Positions is Positive
      range Line_Length + 1 .. Line_Length + End_of_Line'Length;

   Line_Buffer : String (1 .. Line_Length + End_of_Line'Length);

   Nucleo_Cumulative : array (Nucleotide_Index) of Nucleotide;

   procedure Make_Random_Fasta
     (Title       : in String; 
      Nucleotides : in Nucleotide_Set; 
      N           : in Positive)
   is
      function Random_Nucleotide return Character is
         r : constant Real := Gen_Random (1.0);
         Result : Character := 'J';
      begin
         Choose_Random: 
         for i in Nucleo_Cumulative'Range loop
            if Nucleo_Cumulative(i).P > r then
               Result := Nucleo_Cumulative(i).C;
               exit Choose_Random;
            end if;
         end loop Choose_Random;
         return Result;
      end Random_Nucleotide;

      Sum  : Real;
      Remaining_Chars  : constant Natural := N mod Line_Length;
      No_of_Full_Lines : constant Natural := N  /  Line_Length;
   begin
      Line_IO.Print (Title & End_of_Line);

      Nucleo_Cumulative := (others => ('j', 2.0));
      for k in Nucleotides'Range loop
         Nucleo_Cumulative(k).C := Nucleotides(k).C;
      end loop;

      Sum := 0.0;
      for k in Nucleotides'Range loop
         Sum := Sum + Nucleotides(k).P;
         Nucleo_Cumulative(k).P := Sum;
      end loop;

      Line_Buffer(Line_End_Positions) := End_of_Line;

      for k in 1 .. No_of_Full_Lines loop
         for i in 1 .. Line_Length loop
            Line_Buffer(i) := Random_Nucleotide;
         end loop;
         Line_IO.Print (Line_Buffer);
      end loop;

      if Remaining_Chars > 0 then
         for i in 1 .. Remaining_Chars loop
            Line_Buffer(i) := Random_Nucleotide;
         end loop;
         Line_IO.Print (Line_Buffer(1 .. Remaining_Chars) & End_of_Line);
      end if;

   end Make_Random_Fasta;

   procedure Make_Repeat_Fasta 
     (Title : in String;
      S     : in String; 
      N     : in Positive) 
   is
      S_App : constant String := S & S(S'First .. S'First + Line_Length);

      Pos : Positive := S_App'First;
      Remaining_Chars : Natural := N;
      No_of_Chars_Output : Natural := 0;
   begin
      Line_IO.Print (Title & End_of_Line);

      while Remaining_Chars > 0 loop

         No_of_Chars_Output := Integer'Min (Remaining_Chars, Line_Length);

         Line_IO.Print (S_App (Pos .. Pos + No_of_Chars_Output - 1));
         Line_IO.Print (End_of_Line);

         Remaining_Chars := Remaining_Chars - No_of_Chars_Output;

         Pos := Pos + No_of_Chars_Output;
         if Pos > S'Last then
            Pos := Pos - S'Length;
         end if;

      end loop;

   end Make_Repeat_Fasta;

end Sequence.Creation;

package Line_IO is

   procedure Print (Item : String);

   procedure Close_Stdout;

   procedure Open_Stdout;

   End_of_Line : constant String := (1 => ASCII.LF);

   pragma Inline (Print);

end Line_IO;


with Ada.Streams.Stream_IO;
with Unchecked_Conversion;

package body Line_IO is

   use Ada.Streams;

   Stdout : Stream_IO.File_Type;

   procedure Print (Item : String) is
      subtype Index is Stream_Element_Offset range
         Stream_Element_Offset(Item'First) .. Stream_Element_Offset(Item'Last);
      subtype XString is String (Item'Range);
      subtype XBytes is Stream_Element_Array (Index);
      function To_Bytes is new Unchecked_Conversion
        (Source => XString,
         Target => XBytes);
   begin
      Stream_IO.Write (Stdout, To_Bytes (Item));
   end Print;

   procedure Close_Stdout is
   begin
      Stream_IO.Close (Stdout);
   end Close_Stdout;

   procedure Open_Stdout is
   begin
      Stream_IO.Open 
        (File => Stdout,
         Mode => Stream_IO.Out_File,
         Name => "/dev/stdout");
   end Open_Stdout;

end Line_IO;


generic

   type Real is digits <>;

package LCG_Random is

   function Gen_Random (Max : in Real) return Real;
   -- Linear congruential random number generator. 
   -- Period = 139_968, with output in [0.0, 1.0) if Max = 1.0.

end LCG_Random;

package body LCG_Random is

   pragma Assert (Real'Digits > 5);

   type Random_State is mod 2**32;

   State : Random_State := 42;

   type Signed is range
      -2**(Random_State'Size-1) .. 2**(Random_State'Size-1) - 1;

   function Gen_Random (Max : in Real) return Real is
      IM : constant := 139_968;
      IA : constant :=   3_877;
      IC : constant :=  29_573;
   begin
      State := (State * IA + IC) mod IM;
      return (Max * Real (Signed (State))) * (1.0 / Real (IM));
   end Gen_Random;

end LCG_Random;