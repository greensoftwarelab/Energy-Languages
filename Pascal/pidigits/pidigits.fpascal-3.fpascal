{ The Computer Language Benchmark Game
  http://benchmarksgame.alioth.debian.org

  contributed by Le Duc Hieu
  a translation of the C program contributed by Mr Ledhug
}

{$OPTIMIZATION LOOPUNROLL,LEVEL3}
{$INLINE+}

Uses GMP;

Var
  tmp1, tmp2, acc, den, num: MPZ_T;
  // Main
  d, k, i: LongWord;
  n      : LongInt;

Function ExtractDigit(nth: LongWord): LongWord; inline;
  Begin
    // joggling between tmp1 and tmp2, so GMP don't have to use temp buffers
    mpz_mul_ui(tmp1, num, nth);
    mpz_add(tmp2, tmp1, acc);
    mpz_tdiv_q(tmp1, tmp2, den);

    ExtractDigit := mpz_get_ui(tmp1)
  End;

Procedure EliminateDigit(d: LongWord); inline;
  Begin
    mpz_submul_ui(acc, den, d);
    mpz_mul_ui(acc, acc, 10);
    mpz_mul_ui(num, num, 10);
  End;

Procedure NextTerm(k: LongWord);
  Var k2: LongWord;

  Begin
    k2 := k * 2 + 1;

    mpz_addmul_ui(acc, num, 2);
    mpz_mul_ui(acc, acc, k2);
    mpz_mul_ui(den, den, k2);
    mpz_mul_ui(num, num, k)
  End;

Begin
  val(ParamStr(1), n);

  mpz_init(tmp1);
  mpz_init(tmp2);

  mpz_init_set_ui(acc, 0);
  mpz_init_set_ui(den, 1);
  mpz_init_set_ui(num, 1);

  i := 0;
  k := 0;
  while (i < n) do begin
    inc(k);
    NextTerm(k);
    if mpz_cmp(num, acc) > 0 then continue;

    d := ExtractDigit(3);
    if d <> ExtractDigit(4) then continue;

    write(chr(ord('0') + d));
    inc(i);
    if (i mod 10 = 0) or (i = n) then writeln(#9+':', i);
    EliminateDigit(d)
  end
End.