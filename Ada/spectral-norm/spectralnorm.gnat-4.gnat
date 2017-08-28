-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Contributed by Jim Rogers
-- Modified by Jonathan Parker (Oct 2009)
-- Updated by Jonathan Parker and Georg Bauhaus (May 2012)

with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Command_Line; use Ada.Command_Line;
with Spectral_Utils, Spectral_Utils.Dist;
with Division;

procedure SpectralNorm is

   No_of_Cores_to_Use : constant := 4;

   subtype Real is Division.SSE_Real;
   use type Real;

   package Real_IO is new Ada.Text_IO.Float_IO (Real);
   package Real_Funcs is new Ada.Numerics.Generic_Elementary_Functions (Real);
   use Real_Funcs;

   N : Positive := 100;
   Vbv, Vv : Real := 0.0;
begin
   if Argument_Count = 1 then
      N := Positive'Value (Argument(1));
   else
      raise Program_Error;
   end if;

   declare
      package Spectrum is new Spectral_Utils
        (Matrix_Order => N);
      package Calc is new Spectrum.Dist
        (Number_Of_Tasks => No_of_Cores_to_Use);
      use Spectrum, Calc;
      Calculator : constant Matrix_Computation'Class := Make_Calculator;
      U : Matrix := (others => 1.0);
      V : Matrix := (others => 0.0);
   begin
      for I in 1 .. 10 loop
         Eval_Ata_Times_U (Calculator, U, V);
         Eval_Ata_Times_U (Calculator, V, U);
      end loop;
      for I in V'Range loop
         Vbv := Vbv + U(I) * V(I);
         Vv  := Vv  + V(I) * V(I);
      end loop;
   end;
   Real_IO.Put(Item => Sqrt(Vbv/Vv), Fore => 1, Aft => 9, Exp => 0);
   Ada.Text_Io.New_Line;
end SpectralNorm;

with System;
with Division;

generic
   Matrix_Order : Positive;
package Spectral_Utils is

   subtype Real is Division.SSE_Real;

   type Matrix_Index is mod 2**System.Word_Size;

   Matrix_Size : constant Matrix_Index := Matrix_Index (Matrix_Order);

   type Matrix is array(Matrix_Index range 0 .. Matrix_Size-1) of Real;
   -- Matrix is m_ij = 1 / ((i+j+1)*(i+j))/2 + i + 1); indices start at 0.

   type Matrix_Computation is abstract tagged limited null record;

   function Make_Calculator return Matrix_Computation'Class;
   --  adaptive computations

   --  Get   AU = A * U.   Calculate only AU(Start .. Finish).

   procedure Eval_A_Times
     (Iron          : in     Matrix_Computation;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      AU            :    out Matrix) is abstract;

   --  Get   AU = A_transpose * U.   Calculate only AU(Start .. Finish).

   procedure Eval_At_Times
     (Iron          : in     Matrix_Computation;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      AU            :    out Matrix) is abstract;

end Spectral_Utils;

with Spectral_Utils.S, Spectral_Utils.D;

package body Spectral_Utils is

   package Plain is new Spectral_Utils.D;
   package Fancy is new Spectral_Utils.S;

   function Make_Calculator return Matrix_Computation'Class is
   begin
      if System.Word_Size = 64 then
         return Plain.Vanilla'(Matrix_Computation with null record);
      else
         return Fancy.Forced'(Matrix_Computation with null record);
      end if;
   end Make_Calculator;

end Spectral_Utils;

generic package Spectral_Utils.D is

   type Vanilla is new Matrix_Computation with null record;
   -- computations use Division.D

   overriding
   procedure Eval_A_Times
     (Iron          : in     Vanilla;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      AU            :    out Matrix);

   overriding
   procedure Eval_At_Times
     (Iron          : in     Vanilla;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      AU            :    out Matrix);

end Spectral_Utils.D;

with Division.D;

package body Spectral_Utils.D is

   use type Real;
   use Division.D;

   subtype A_Element_Pair is SSE_Vector;

   --  A is the matrix. Evaluate matrix A at (I,J):

   function Eval_A (I, J : in Matrix_Index) return Real is
      Denom : constant Real := Real (((I + J) * (I + J + 1)) / 2 + I + 1);
   begin
      return 1.0 / Denom;
   end Eval_A;

   function Eval_A_tr_Twice (I, J : in Matrix_Index) return A_Element_Pair is
      y0 : constant Real := Real (((I + J    )*(I + J + 1))/2 + J + 1);
      y1 : constant Real := Real (((I + J + 1)*(I + J + 2))/2 + J + 2);
   begin
      return Division.D.Ratios (1.0, 1.0, y0, y1);
   end Eval_A_tr_Twice;

   function Eval_A_Twice (I, J : in Matrix_Index) return A_Element_Pair is
      y0 : constant Real := Real (((I + J    )*(I + J + 1))/2 + I + 1);
      y1 : constant Real := Real (((I + J + 1)*(I + J + 2))/2 + I + 1);
   begin
      return Division.D.Ratios (1.0, 1.0, y0, y1);
   end Eval_A_Twice;

   Half_Matrix_Size : constant Matrix_Index := Matrix_Size / 2;

   overriding
   procedure Eval_A_Times
     (Iron          : in     Vanilla;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      Au            :    out Matrix)
   is
      J_Index : Matrix_Index;
      A_Elements : A_Element_Pair;
      Sum : Real;
   begin
      for I in Start .. Finish loop
         Sum := 0.0;
         for J in Matrix_Index range 0 .. Half_Matrix_Size - 1 loop
            J_Index    := U'First + 2*J;
            A_Elements := Eval_A_Twice (I, J_Index);
            Sum := Sum + A_Elements(0) * U(J_Index) + A_Elements(1) * U(J_Index+1);
         end loop;
         if Matrix_Size mod 2 = 1 then
            Sum := Sum + Eval_A (U'Last, I) * U(U'Last); -- J_Index := U'Last;
         end if;
         Au(I) := Sum;
      end loop;
   end Eval_A_Times;

   overriding
   procedure Eval_At_Times
     (Iron          : in     Vanilla;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      Au            :    out Matrix)
   is
      J_Index : Matrix_Index;
      A_Elements : A_Element_Pair;
      Sum : Real;
   begin
      for I in Start .. Finish loop
         Sum := 0.0;
         for J in Matrix_Index range 0 .. Half_Matrix_Size - 1 loop
            J_Index    := U'First + 2*J;
            A_Elements := Eval_A_tr_Twice (I, J_Index);
            Sum := Sum + A_Elements(0) * U(J_Index) + A_Elements(1) * U(J_Index+1);
         end loop;
         if Matrix_Size mod 2 = 1 then
            Sum := Sum + Eval_A (U'Last, I) * U(U'Last); -- J_Index := U'Last;
         end if;
         Au(I) := Sum;
      end loop;
   end Eval_At_Times;

end Spectral_Utils.D;

generic package Spectral_Utils.S is

   type Forced is new Matrix_Computation with null record;
   -- computations use Division.S

   overriding
   procedure Eval_A_Times
     (Iron          : in     Forced;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      AU            :    out Matrix);

   overriding
   procedure Eval_At_Times
     (Iron          : in     Forced;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      AU            :    out Matrix);

end Spectral_Utils.S;

with Division.S;
package body Spectral_Utils.S is

   use type Real;
   use Division.S;

   subtype A_Element_Pair is SSE_Vector;

   --  A is the matrix. Evaluate matrix A at (I,J):

   function Eval_A (I, J : in Matrix_Index) return Real is
      Denom : constant Real := Real (((I + J) * (I + J + 1)) / 2 + I + 1);
   begin
      return 1.0 / Denom;
   end Eval_A;

   function Eval_A_tr_Twice (I, J : in Matrix_Index) return A_Element_Pair is
      Denoms : constant A_Element_Pair:=
        (Real (((I + J    )*(I + J + 1))/2 + J + 1),
         Real (((I + J + 1)*(I + J + 2))/2 + J + 2));
   begin
      return (1.0, 1.0) / Denoms;
   end Eval_A_tr_Twice;

   function Eval_A_Twice (I, J : in Matrix_Index) return A_Element_Pair is
      Denoms : constant A_Element_Pair :=
        (Real (((I + J    )*(I + J + 1))/2 + I + 1),
         Real (((I + J + 1)*(I + J + 2))/2 + I + 1));
   begin
      return (1.0, 1.0) / Denoms;
   end Eval_A_Twice;

   Half_Matrix_Size : constant Matrix_Index := Matrix_Size / 2;

   overriding
   procedure Eval_A_Times
     (Iron          : in     Forced;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      Au            :    out Matrix)
   is
      J_Index : Matrix_Index;
      Elements : array (Matrix_Index range 0 .. Matrix_Size / 2) of SSE_Vector;
      Sums : SSE_Vector;
   begin
      for I in Start .. Finish loop
         Sums := (0.0, 0.0);
         for J in Matrix_Index range 0 .. Half_Matrix_Size - 1 loop
            J_Index      := U'First + 2*J;
            Elements (J) := Eval_A_Twice (I, J_Index);
         end loop;
         for J in Matrix_Index range 0 .. Half_Matrix_Size - 1 loop
            J_Index      := U'First + 2*J;
            Sums := Sums + Elements(J) * (U(J_Index), U(J_Index+1));
         end loop;
         if Matrix_Size mod 2 = 1 then
            Sums(0) := Sums(0) + Eval_A(I, U'Last) * U(U'Last); -- J_Index := U'Last;
         end if;
         Au(I) := Sums(0) + Sums(1);
      end loop;
   end Eval_A_Times;

   overriding
   procedure Eval_At_Times
     (Iron          : in     Forced;
      U             : in     Matrix;
      Start, Finish : in     Matrix_Index;
      Au            :    out Matrix)
   is
      J_Index : Matrix_Index;
      Elements : array (Matrix_Index range 0 .. Matrix_Size / 2) of SSE_Vector;
      Sums : SSE_Vector;
   begin
      for I in Start .. Finish loop
         Sums := (0.0, 0.0);
         for J in Matrix_Index range 0 .. Half_Matrix_Size - 1 loop
            J_Index     := U'First + 2*J;
            Elements(J) := Eval_A_tr_Twice (I, J_Index);
         end loop;
         for J in Matrix_Index range 0 .. Half_Matrix_Size - 1 loop
            J_Index     := U'First + 2*J;
            Sums := Sums + Elements(J) * (U(J_Index), U(J_Index+1));
         end loop;
         if Matrix_Size mod 2 = 1 then
            Sums(0) := Sums(0) + Eval_A (U'Last, I) * U(U'Last); -- J_Index := U'Last;
         end if;
         Au(I) := Sums(0) + Sums(1);
      end loop;
   end Eval_At_Times;

end Spectral_Utils.S;

generic
   Number_Of_Tasks : Positive;
package Spectral_Utils.Dist is

   --  distribute the computation

   pragma Elaborate_Body (Dist);

   --  Get   A_transpose_A_times_U = A_transpose * A * U.

   procedure Eval_Ata_Times_U
     (Iron                  : in     Matrix_Computation'Class;
      U                     : in     Matrix;
      A_transpose_A_times_U :    out Matrix);

private
   No_of_Tasks : constant Matrix_Index := Matrix_Index (Number_Of_Tasks);

   -- Calculate A * U

   task type Matrix_A_times_U is
      pragma Storage_Size (2**18);
      entry Multiply (U : in Matrix; Start, Finish : in Matrix_Index);
      entry Result (Start, Finish : out Matrix_Index; R : out Matrix);
   end Matrix_A_times_U;

   -- Calculate A_transpose * V

   task type Matrix_A_tr_times_V is
      pragma Storage_Size (2**18);
      entry Multiply (V : in Matrix; Start, Finish : in Matrix_Index);
      entry Result (Start, Finish : out Matrix_Index; R : out Matrix);
   end Matrix_A_tr_times_V;

end Spectral_Utils.Dist;

package body Spectral_Utils.Dist is

   -- Create (No_Of_Tasks-1) tasks. The final task is the environmental task,
   -- which does its fair share of the work in procedure Eval_Ata_Times_U.

   subtype Task_Range is Matrix_Index range 1 .. No_Of_Tasks-1;

   Partial_Matrix_A_times_U    : array (Task_Range) of Matrix_A_times_U;
   Partial_Matrix_A_tr_times_V : array (Task_Range) of Matrix_A_tr_times_V;

   procedure Eval_Ata_Times_U
      (Iron                  : in     Matrix_Computation'Class;
       U                     : in     Matrix;
       A_transpose_A_times_U :    out Matrix)
   is
      V, Partial_Product : Matrix;

      Segment_Length : constant Matrix_Index := Matrix_Size / No_Of_Tasks;

      I1, I2, J1, J2 : Matrix_Index;
   begin
      I1 := V'First;
      I2 := V'First + Segment_Length - 1;
      I2 := Matrix_Index'Min (I2, V'Last);

      -- Start running the tasks in Task_Range:

      for k in Task_Range loop
         Partial_Matrix_A_times_U(k).Multiply (U, I1, I2);
         I1 := I2 + 1;
         I2 := I2 + Segment_Length;
         I2 := Matrix_Index'Min (I2, V'Last);
      end loop;

      Iron.Eval_A_Times (U, I1, V'Last, V); -- Env task updates V(I1 .. V'Last).

      -- Rendezvous with tasks to get partial results. Write results to V:

      for k in Task_Range loop
         Partial_Matrix_A_times_U(k).Result (J1, J2, Partial_Product);
         V(J1 .. J2) := Partial_Product(J1 .. J2);
      end loop;

      -- The result, stored in V, is A*U. Next get A_transpose * (A*U).

      I1 := V'First;
      I2 := V'First + Segment_Length - 1;
      I2 := Matrix_Index'Min (I2, V'Last);

      for k in Task_Range loop
         Partial_Matrix_A_tr_times_V(k).Multiply (V, I1, I2);
         I1 := I2 + 1;
         I2 := I2 + Segment_Length;
         I2 := Matrix_Index'Min (I2, V'Last);
      end loop;

      Iron.Eval_At_Times (V, I1, V'Last, A_transpose_A_times_U);
      -- Env. task updates A_transpose_A_times_U (I1 .. V'Last).

      for k in Task_Range loop
         Partial_Matrix_A_tr_times_V(k).Result (J1, J2, Partial_Product);
         A_transpose_A_times_U(J1 .. J2) := Partial_Product(J1 .. J2);
      end loop;

   end Eval_Ata_Times_U;

   task body Matrix_A_times_U is
      I1, I2 : Matrix_Index;
      AU, U_local : Matrix;
      Calculator : constant Matrix_Computation'Class := Make_Calculator;
   begin
     loop
     select
        accept Multiply (U : in Matrix; Start, Finish : in Matrix_Index) do
           I1 := Start;
           I2 := Finish;
           U_local := U;
        end Multiply;

        Calculator.Eval_A_Times (U_Local, I1, I2, AU); -- updates AU(I1..I2)

        accept Result (Start, Finish : out Matrix_Index; R : out Matrix) do
           Start  := I1;
           Finish := I2;
           R(Start .. Finish) := AU(Start .. Finish);
        end Result;
     or
        terminate;
     end select;
     end loop;
   end Matrix_A_times_U;

   task body Matrix_A_tr_times_V is
      I1, I2 : Matrix_Index;
      AV, V_local : Matrix;
      Calculator : constant Matrix_Computation'Class := Make_Calculator;
   begin
     loop
     select
        accept Multiply (V : in Matrix; Start, Finish : in Matrix_Index) do
           I1 := Start;
           I2 := Finish;
           V_local := V;
        end Multiply;

        Calculator.Eval_At_Times (V_Local, I1, I2, AV);
        -- AV = A_transpose * V_local

        accept Result (Start, Finish : out Matrix_Index; R : out Matrix) do
           Start  := I1;
           Finish := I2;
           R(Start .. Finish) := AV(Start .. Finish);
        end Result;
     or
        terminate;
     end select;
     end loop;
   end Matrix_A_tr_times_V;

end Spectral_Utils.Dist;

package Division is

   pragma Pure (Division);

   type SSE_Real is new Long_Float;

private
   pragma Assert (SSE_Real'Size = 64 and SSE_Real'digits > 13);
end Division;


package Division.S is

   --  force SSE operations

   type SSE_Vector is array (0 .. 1) of SSE_Real;
   for SSE_Vector'Alignment use 16;
   pragma Machine_Attribute (SSE_Vector, "vector_type");
   pragma Machine_Attribute (SSE_Vector, "may_alias");

   function "+" (X, Y : SSE_Vector) return SSE_Vector;
   function "*" (X, Y : SSE_Vector) return SSE_Vector;
   function "/" (X, Y : SSE_Vector) return SSE_Vector;

private
   pragma Import (Intrinsic, "+", "__builtin_ia32_addpd");
   pragma Import (Intrinsic, "*", "__builtin_ia32_mulpd");
   pragma Import (Intrinsic, "/", "__builtin_ia32_divpd");
end Division.S;


package Division.D is

   type SSE_Vector is array(0 .. 1) of SSE_Real;

   function Ratios (x0, x1, y0, y1 : in SSE_Real) return SSE_Vector;

end Division.D;

package body Division.D is

   type m128d is array (0 .. 1) of SSE_Real;
   for m128d'Alignment use 16;
   pragma Machine_Attribute (m128d, "vector_type");
   pragma Machine_Attribute (m128d, "may_alias");

   function Div (X, Y : m128d) return m128d;
   pragma Import (Intrinsic, Div, "__builtin_ia32_divpd");

   function Ratios (x0, x1, y0, y1 : in SSE_Real) return SSE_Vector is
      z : constant m128d := Div ((x0, x1), (y0, y1));
   begin
      return (z(0), z(1));
   end Ratios;

end Division.D;