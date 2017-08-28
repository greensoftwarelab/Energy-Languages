-----------------------
-- nbody.adb
-----------------------
-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org
--
-- Contributed by Brian Kolden
-- Ada port of Mark C. Lewis's N-Body
-- Built off of Pascal Obry's N-Body

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Nbody_Pck;   use Nbody_Pck;
with Root; use Root;
procedure Nbody is
   pragma Suppress(All_Checks);

   N : constant Positive := Integer'Value (Argument (1));

   package RIO is new Ada.Text_IO.Float_Io (S_Real);

   procedure Put
     (Item : S_Real; Fore : Field := 0; Aft : Field := 9;
      Exp  : Field := 0) renames RIO.Put;

begin
   Put (Energy);
   New_Line;

   for I in 1 .. N loop
      Advance (0.01);
   end loop;

   Put (Energy);
   New_Line;

end Nbody;

-----------------------
-- nbody_pck.ads
-----------------------
with Systems; use Systems;
with Ada.Text_IO;      use Ada.Text_IO;
with Root; use Root;
package Nbody_Pck is
   pragma Elaborate_Body;
   pragma Suppress(All_Checks);
   
   type R is record
      DX, DY, DZ, Filler: S_Real;
   end record;
   for R'Alignment use 16;
   type R_Array_Type is array(0..999) of R;

   type Mag_Type is array (0..999) of S_Real;
   for Mag_Type'Alignment use 16;

   PX, PY, PZ: S_Real;
   Bodies: array(0..4) of System:=(Sun, Jupiter, Saturn, Uranus, Neptune);

   procedure Advance(DT: S_Real);
   function Energy return S_Real;

end Nbody_Pck;

-----------------------
-- nbody_pck.adb
-----------------------
package body Nbody_Pck is

   procedure Advance(DT: S_Real) is
      N: constant Integer := (Bodies'Length-1)*Bodies'Length/2;      
      R_Array : R_Array_Type;      
      Mag : Mag_Type;
      K: Integer := 0;
   begin
      for I in Bodies'First..Bodies'Last-1 loop
         for J in I+1..Bodies'Last loop
            R_Array(K).DX := Bodies(I).X - Bodies(J).X;
            R_Array(K).DY := Bodies(I).Y - Bodies(J).Y;
            R_Array(K).DZ := Bodies(I).Z - Bodies(J).Z;
            
            K := K + 1;
         end loop;
      end loop;

      declare 
         DX, DY, DZ, D_Squared, Distance, dmag: m128d;
      begin
         K:= 0;
         while K < N loop
            DX(0):= R_Array(K).DX;
            DY(0):= R_Array(K).DY;
            DZ(0):= R_Array(K).DZ;
            DX(1):= R_Array(K+1).DX;
            DY(1):= R_Array(K+1).DY;
            DZ(1):= R_Array(K+1).DZ;

            D_Squared:= ia32_Mul(DX, DX) + ia32_Mul(DY, DY) + ia32_Mul(DZ, DZ);

            Distance:= ia32_CvtPS_PD(ia32_RSqrt(ia32_CvtPD_PS(D_Squared)));

            for J in 1..2 loop
               Distance:= Distance * (1.5, 1.5) -
                  (((0.5, 0.5) * D_Squared) * Distance) *
                  (Distance * Distance);
            end loop;

            dmag := ia32_Div((DT, DT), D_Squared) * Distance;

            Mag(K):= dmag(0);
            Mag(K+1):= dmag(1);

            K:= K + 2;
         end loop;         
      end;

      K:= 0;
      for I in Bodies'First..Bodies'Last-1 loop
         for J in I+1..Bodies'Last loop
            Bodies(I).VX:= Bodies(I).VX - (R_Array(K).DX * Bodies(J).Mass * Mag(K));
            Bodies(I).VY:= Bodies(I).VY - (R_Array(K).DY * Bodies(J).Mass * Mag(K));
            Bodies(I).VZ:= Bodies(I).VZ - (R_Array(K).DZ * Bodies(J).Mass * Mag(K));

            Bodies(J).VX := Bodies(J).VX + (R_Array(K).DX * Bodies(I).Mass * Mag(K));
            Bodies(J).VY := Bodies(J).VY + (R_Array(K).DY * Bodies(I).Mass * Mag(K));
            Bodies(J).VZ := Bodies(J).VZ+ (R_Array(K).DZ * Bodies(I).Mass * Mag(K));

            K := K + 1;
         end loop;
      end loop;


      for I of Bodies loop

         I.X := I.X + (DT * I.VX);
         I.Y := I.Y + (DT * I.VY);
         I.Z := I.Z + (DT * I.VZ);

      end loop;

   end Advance;

   function Energy return S_Real is
      E: S_Real := 0.0;
      DX, DY, DZ, Distance : S_Real;
   begin
      for I in Bodies'Range loop
         E := E + (0.5 * Bodies(I).Mass *
                ( Bodies(I).VX * Bodies(I).VX
                    + Bodies(I).VY * Bodies(I).VY
                    + Bodies(I).VZ * Bodies(I).VZ ));
         for J in I+1..Bodies'Last loop
            declare
               J_Body: constant System := Bodies(J);
            begin
               DX := Bodies(I).X - J_Body.X;
               DY := Bodies(I).Y - J_Body.Y;
               DZ := Bodies(I).Z - J_Body.Z;

               Distance := Sqrt(DX*DX + DY*DY + DZ*DZ);
               E := E - ((Bodies(I).Mass * J_Body.Mass) / Distance);
            end;
         end loop;
      end loop;
      return E;
   end Energy;

begin
   PX := 0.0;
   PY :=0.0; 
   PZ := 0.0;

   for I of Bodies loop
      PX := PX + (I.VX * I.Mass);
      PY := PY + (I.VY * I.Mass);
      PZ := PZ + (I.VZ * I.Mass);
   end loop;
   Systems.Offset_Momentum(Bodies(0), PX, PY, PZ);
   
end Nbody_Pck;

-----------------------
-- systems.ads
-----------------------
with Root; use Root;
package Systems is
   pragma Suppress(All_Checks);
   
   type System is record
      X, Y, Z: S_Real:=0.0;
      Filler: S_Real;
      VX, VY, VZ: S_Real;
      Mass: S_Real;
   end record;

   PI: constant S_Real := 3.141592653589793;
   SOLAR_MASS: constant S_Real := 4.0 * PI * PI;
   DAYS_PER_YEAR: constant S_Real := 365.24;

   Jupiter: System:= (
      X => 4.84143144246472090e+00,
      Y => -1.16032004402742839e+00,
      Z => -1.03622044471123109e-01,
      VX => 1.66007664274403694e-03 * DAYS_PER_YEAR,
      VY => 7.69901118419740425e-03 * DAYS_PER_YEAR,
      VZ => -6.90460016972063023e-05 * DAYS_PER_YEAR,
      Mass => 9.54791938424326609e-04 * SOLAR_MASS,
      Others => 0.0);
   Saturn: System:= (
      X => 8.34336671824457987e+00,
      Y => 4.12479856412430479e+00,
      Z => -4.03523417114321381e-01,
      VX => -2.76742510726862411e-03 * DAYS_PER_YEAR,
      VY => 4.99852801234917238e-03 * DAYS_PER_YEAR,
      VZ => 2.30417297573763929e-05 * DAYS_PER_YEAR,
      Mass => 2.85885980666130812e-04 * SOLAR_MASS,
      Others => 0.0);
   Uranus: System:= (
      X => 1.28943695621391310e+01,
      Y => -1.51111514016986312e+01,
      Z => -2.23307578892655734e-01,
      VX => 2.96460137564761618e-03 * DAYS_PER_YEAR,
      VY => 2.37847173959480950e-03 * DAYS_PER_YEAR,
      VZ => -2.96589568540237556e-05 * DAYS_PER_YEAR,
      Mass => 4.36624404335156298e-05 * SOLAR_MASS,
      Others => 0.0);
   Neptune: System:= (
      X => 1.53796971148509165e+01,
      Y => -2.59193146099879641e+01,
      Z => 1.79258772950371181e-01,
      VX => 2.68067772490389322e-03 * DAYS_PER_YEAR,
      VY => 1.62824170038242295e-03 * DAYS_PER_YEAR,
      VZ => -9.51592254519715870e-05 * DAYS_PER_YEAR,
      Mass => 5.15138902046611451e-05 * SOLAR_MASS,
      Others => 0.0);
   Sun: System:= (
      Mass => SOLAR_MASS,
      Others => 0.0);

   procedure Offset_Momentum (This: in out System; PX,PY, PZ: S_Real);
   pragma Inline(Offset_Momentum);

end Systems;

-----------------------
-- systems.adb
-----------------------
package body Systems is

   procedure Offset_Momentum (This: in out System; PX,PY, PZ: S_Real) is
   begin
      This.VX := -PX / SOLAR_MASS;
      This.VY := -PY / SOLAR_MASS;
      This.VZ := -PZ / SOLAR_MASS;
   end Offset_Momentum;

end Systems;   

-----------------------
-- root.ads
-----------------------
package Root is

   pragma Pure;
   pragma Suppress(All_Checks);

   type S_Real is new Long_Float;

   pragma Assert (S_Real'Size = 64 and S_Real'digits > 13);

   type m128d is array (0 .. 1) of S_Real;
   for m128d'Alignment use 16;
   pragma Machine_Attribute (m128d, "vector_type");

   type m128s is array (0 .. 3) of Float;
   for m128s'Alignment use 16;
   pragma Machine_Attribute (m128s, "vector_type");
   pragma Assert (Float'Digits < 7 and m128s'size = 128);

   function ia32_Div (X, Y : m128d) return m128d;
   pragma Import (Intrinsic, ia32_Div, "__builtin_ia32_divpd");
   pragma Inline(ia32_Div);

   function ia32_Sqrt (X : m128d) return m128d;
   pragma Import (Intrinsic, ia32_Sqrt, "__builtin_ia32_sqrtpd");
   pragma Inline(ia32_Sqrt);

   function ia32_Mul (X, Y : m128d) return m128d;
   pragma Import (Intrinsic, ia32_Mul, "__builtin_ia32_mulpd");
   pragma Inline(ia32_Mul);   

   function ia32_Add (X, Y : m128d) return m128d;
   pragma Import (Intrinsic, ia32_Add, "__builtin_ia32_addpd");
   pragma Inline(ia32_Add);

   function ia32_Sub (X, Y : m128d) return m128d;
   pragma Import (Intrinsic, ia32_Sub, "__builtin_ia32_subpd");
   pragma Inline(ia32_Sub);

   function Sqrt (X : S_Real) return S_Real;
   pragma Inline(Sqrt);

   function "*" (Left, Right: m128d) return m128d is(
      ia32_Mul(Left, Right));
   function "+" (Left, Right: m128d) return m128d is(
      ia32_Add(Left, Right));
   function "-" (Left, Right: m128d) return m128d is(
      ia32_Sub(Left, Right));

   function ia32_RSqrt (X : m128s) return m128s;
   pragma Import (Intrinsic, ia32_RSqrt, "__builtin_ia32_rsqrtps");
   pragma Inline(ia32_RSqrt);

   function ia32_CvtPD_PS(X: m128d) return m128s;
   pragma Import (Intrinsic, ia32_CvtPD_PS, "__builtin_ia32_cvtpd2ps");
   pragma Inline(ia32_CvtPD_PS);

   function ia32_CvtPS_PD(X: m128s) return m128d;
   pragma Import (Intrinsic, ia32_CvtPS_PD, "__builtin_ia32_cvtps2pd");
   pragma Inline(ia32_CvtPS_PD);

   function ia32_Set(X: S_Real) return m128d;
   pragma Import (Intrinsic, ia32_Set, "__builting_ia32_set1pd");
   pragma Inline(ia32_Set);

   x : constant m128d := (4.0, 6.0);
   y : constant m128d := (2.0, 2.0);

end Root;

-----------------------
-- root.adb
-----------------------
package body root is

   function Sqrt (X : S_Real) return S_Real is
   begin 
      return ia32_Sqrt ((X, 1.0))(0);
   end Sqrt;

end root;