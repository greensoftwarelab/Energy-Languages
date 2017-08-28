--  The Computer Language Benchmarks Game
--  http://benchmarksgame.alioth.debian.org
--  Calculate digits of pi using the
--  Unbounded Spigot Algorithms
--
--  Contributed by Brian Kolden
--  gmp headers by Karl-Michael Schindler and Brian Kolden
--  Port of Mr Ledrug's pidigits

with Ada.Command_Line;    use Ada.Command_Line;

with Interfaces.C;

procedure Pidigits is

  pragma Suppress(All_Checks);

  package GMP_Mini is
  	Print_String : constant String := ASCII.HT & ":%d" & ASCII.LF & ASCII.NUL;

    type mp_limb_t is new Interfaces.C.unsigned;
    type mp_ptr is access mp_limb_t;

    type mpz_t is record
      mp_alloc, mp_size : Interfaces.C.int;
      mp_d    : mp_ptr;
    end record;

    procedure mpz_init (Dest: out mpz_t) with Inline=>True;
    pragma Import(C, mpz_init, "__gmpz_init");

    procedure mpz_init_set_ui (Dest: out mpz_t; Src: in Interfaces.C.unsigned_long) with Inline=>True;
    pragma Import(C, mpz_init_set_ui, "__gmpz_init_set_ui");

    procedure mpz_mul_ui (Dest: out mpz_t; Src1: in mpz_t; Src2: in Interfaces.C.unsigned_long) with Inline=>True;
    pragma Import(C, mpz_mul_ui, "__gmpz_mul_ui");

    procedure mpz_add (Dest: out mpz_t; Src1, Src2: in mpz_t) with Inline=>True;
    pragma Import(C, mpz_add, "__gmpz_add");

    procedure mpz_addmul_ui (Dest: out mpz_t; Src1: in mpz_t; Src2: in Interfaces.C.unsigned) with Inline=>True;
    pragma Import(C, mpz_addmul_ui, "__gmpz_addmul_ui");

    procedure mpz_submul_ui (Dest: out mpz_t; Src1: in mpz_t; Src2: in Interfaces.C.unsigned) with Inline=>True;
    pragma Import(C, mpz_submul_ui, "__gmpz_submul_ui");

    procedure mpz_tdiv_q (Dest: out mpz_t; Src1, Src2: in mpz_t) with Inline=>True;
    pragma Import(C, mpz_tdiv_q, "__gmpz_tdiv_q");

    function mpz_get_ui (Src: in mpz_t) return Interfaces.C.unsigned_long with Inline=>True;
    pragma Import(C, mpz_get_ui, "__gmpz_get_ui");

    function mpz_cmp (Src: in mpz_t; Ref: in mpz_t) return Interfaces.C.int with Inline=>True;
    pragma Import(C, mpz_cmp, "__gmpz_cmp");

    procedure Put_Char (Val : in Interfaces.C.unsigned) with Inline=>True;
    pragma Import(C, Put_Char, "putchar");

    procedure Put_Line (Format : in String; Data : in Interfaces.C.unsigned) with Inline=>True;
    pragma Import(C, Put_Line, "printf");

    pragma Linker_Options("-lgmp");

  end GMP_Mini;
  use GMP_Mini;

  Tmp1, Tmp2, Acc, Den, Num : mpz_t;

  function Extract(X: Interfaces.C.Unsigned_long) return Interfaces.C.unsigned with Inline=>True;
  function Extract(X: Interfaces.C.Unsigned_long) return Interfaces.C.unsigned is
  begin
    mpz_mul_ui(Tmp1, Num, X);
    mpz_add(Tmp2, Tmp1, Acc);
    mpz_tdiv_q(Tmp1, Tmp2, Den);
    return Interfaces.C.unsigned(mpz_get_ui(Tmp1));
  end Extract;

  procedure Eliminate (D : in Interfaces.C.unsigned) with Inline=>True;
  procedure Eliminate (D : in Interfaces.C.unsigned) is
  begin
    mpz_submul_ui(Acc, Den, D);
    mpz_mul_ui(Acc, Acc, 10);
    mpz_mul_ui(Num, Num, 10);
  end Eliminate;

  procedure Consume (K : in Interfaces.C.unsigned) with Inline=>True;
  procedure Consume (K : in Interfaces.C.unsigned) is
    use Interfaces.C;
    K2 : unsigned;
  begin
    K2 := K * 2 + 1;

    mpz_addmul_ui(Acc, Num, 2);
    mpz_mul_ui(Acc, Acc, unsigned_long(K2));
    mpz_mul_ui(Den, Den, unsigned_long(K2));
    mpz_mul_ui(Num, Num, unsigned_long(K));
  end Consume;


  N : Interfaces.C.unsigned;

begin
  N := 2_500;
  if Argument_Count=1 then
    N := Interfaces.C.unsigned'Value(Argument(1));
  end if;

  mpz_init(Tmp1);
  mpz_init(Tmp2);

  mpz_init_set_ui(Acc, 0);
  mpz_init_set_ui(Den, 1);
  mpz_init_set_ui(Num, 1);

  declare
    use Interfaces.C;
    D, K, I : Interfaces.C.unsigned := 0;
  begin
    <<Work_Loop>>
    while I < N loop
      K := K + 1;
      Consume(K);

      if mpz_cmp(Num, Acc) > 0 then
        goto Work_Loop;
      end if;

      D := Extract(3);
      if D /= Extract(4) then 
        goto Work_Loop;
      end if;

      Put_Char(D + 48);
      I := I + 1;
      if I rem 10 = 0 then
        Put_Line(Print_String, I);
      end if;
      Eliminate(D);

    end loop;
  end;

end Pidigits;