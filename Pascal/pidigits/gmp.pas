{
    This file is part of the Free Pascal packages
    Copyright (c) 2009 by Jan Mercl

    An header for the GMP library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit gmp;

{$mode objfpc}{$h+}
{$packrecords c}

//todo:windows link error on GMP global vars, reason not yet known
{$ifdef windows}
{$define NO_GMP_GLOBVARS}
{$endif}

{$ifdef darwin}
  {$linklib gmp.3}
{$endif}
{ Unused symbols exported from GMP:

  Marked preliminary in GMP manual
    __gmpn_bdivmod

  Marked obsolete in GMP manual
    __gmpn_divrem
    __gmpz_random
    __gmpz_random2
    __gmp_randinit

  Not documented in GMP manual
    __gmpf_size
    __gmpn_divrem_2
    __gmpn_pow_1
    __gmpn_preinv_mod_1
    __gmpz_millerrabin

  Marked for use only within GDB
    __gmpf_dump
    __gmpz_dump
}

interface

uses
  sysutils;

const
  BASE10 = 10;
  LIB = 'gmp';
  LOG_10_2 = 0.3010299956639812;
  ERROR_NONE = 0;
  ERROR_UNSUPPORTED_ARGUMENT = 1;
  ERROR_DIVISION_BY_ZERO = 2;
  ERROR_SQRT_OF_NEGATIVE = 4;
  ERROR_INVALID_ARGUMENT = 8;
  RAND_ALG_DEFAULT = 0;
  RAND_ALG_LC = RAND_ALG_DEFAULT;

type

  // ---- GMP types ----

  { low level multi precision integer atom = machine size uint }
  mp_limb_t = valuint;
  { ^array of mp_limb_t}
  mpn_ptr = ^mp_limb_t;
  mp_size_t = sizeint;
  mp_exp_t = valsint;
  randalg_t = longint;

  { multi precision integer number record }
  mpz_t = record
    alloc: longint;
    size: longint;
    data: mpn_ptr;
  end;
  mpz_ptr = ^mpz_t;

  { multi precision rational number record }
  mpq_t = record
    num: mpz_t;
    den: mpz_t;
  end;
  mpq_ptr = ^mpq_t;

  { multi precision real number record }
  mpf_t = record
    prec: longint;
    size: longint;
    exp: mp_exp_t;
    data: mpn_ptr;
  end;
  mpf_ptr = ^mpf_t;

  randstate_t = record
    seed: mpz_t;
    alg: randalg_t;
    algdata: record
      case longint of
        0 : (lc : pointer);
    end;
  end;
  randstate_ptr = ^randstate_t;

  { Return a pointer to newly allocated space with at least alloc size bytes }
  alloc_func_t = function(alloc_size: sizeuint): pointer; cdecl;
  { Resize a previously allocated block ptr of old size bytes to be new size bytes }
  reallocate_func_t = function(p: pointer; old_size, new_size: sizeuint): pointer; cdecl;
  { De-allocate the space pointed to by ptr }
  free_proc_t = procedure(p: pointer; size: sizeuint); cdecl;
  palloc_func = ^alloc_func_t;
  preallocate_func = ^reallocate_func_t;
  pfree_proc = ^free_proc_t;

  // ---- ext types with automatic mem mngmt & cow, ~ fpc string type style -----

  IMPBase = interface
    ['{390336B5-6B78-47E0-BB0B-48F3AF9D5CCC}']
    function refs: longint;
  end;

  MPInteger = interface(IMPBase)
    ['{F6A977E7-B5E6-42BB-981F-E1A7C7EE0D30}']
    function ptr: mpz_ptr;
  end;

  MPFloat = interface(IMPBase)
    ['{73F21043-CC71-425E-9825-1EF0FF4B9B85}']
    function ptr: mpf_ptr;
  end;

  MPRational = interface(IMPBase)
    ['{0ACDDB76-5C1A-48E5-96EF-EA8647680FC1}']
    function ptr: mpq_ptr;
  end;

  MPRandState = interface(IMPBase)
    ['{0E7EDBB9-E009-4A29-8BAC-8B967404B7B7}']
    function ptr: randstate_ptr;
  end;

  { TMPBase }

  TMPBase = class(TInterfacedObject, IMPBase)
  private
    function refs: longint;  inline;
  end;

  { TMPInteger }

  TMPInteger = class(TMPBase, MPInteger)
  private
    fmpz: mpz_t;
    function ptr: mpz_ptr;  inline;
  public
    destructor destroy; override;
  end;

  { TMPFloat }

  TMPFloat = class(TMPBase, MPFloat)
  private
    fmpf: mpf_t;
    function ptr: mpf_ptr;  inline;
  public
    destructor destroy; override;
  end;

  { TMPRational }

  TMPRational = class(TMPBase, MPRational)
  private
    fmpq: mpq_t;
    function ptr: mpq_ptr;  inline;
  public
    destructor destroy; override;
  end;

  { TMPRandState }

  TMPRandState = class(TMPBase, MPRandState)
  private
    frandstate: randstate_t;
    function ptr: randstate_ptr;  inline;
  public
    destructor destroy; override;
  end;

// ==== GMP bindings ====

// ---- Custom Allocation ----

{ Replace the current allocation functions from the arguments }
procedure mp_set_memory_functions(alloc_func_ptr: alloc_func_t; realloc_func_ptr: reallocate_func_t; free_func_ptr: free_proc_t); cdecl; external LIB name '__gmp_set_memory_functions';
{ Get the current allocation functions, storing function pointers to the locations given by the arguments }
procedure mp_get_memory_functions(alloc_func_ptr: palloc_func; realloc_func_ptr: preallocate_func; free_func_ptr: pfree_proc); cdecl; external LIB name '__gmp_get_memory_functions';

// ---- Random Number Functions ----

{ Obsolete: Initialize state with an algorithm selected by alg }
// procedure randinit(var state: randstate_t; alg: randalg_t; args: array of const); cdecl; external LIB name '__gmp_randinit';

{ Initialize state with a default algorithm }
procedure mp_randinit_default(out state: randstate_t); cdecl; external LIB name '__gmp_randinit_default';
{ Initialize state with a linear congruential algorithm X = (aX + c) mod 2^m2exp }
procedure mp_randinit_lc_2exp(out state: randstate_t; var a: mpz_t; c, m2exp: valuint); cdecl; external LIB name '__gmp_randinit_lc_2exp';
{ Initialize state for a linear congruential algorithm as per gmp_randinit_lc_2exp }
function mp_randinit_lc_2exp_size(out state: randstate_t; size: sizeuint): longint; cdecl; external LIB name '__gmp_randinit_lc_2exp_size';
{ Initialize state for a Mersenne Twister algorithm }
procedure mp_randinit_mt(out state: randstate_t); cdecl; external LIB name '__gmp_randinit_mt';
{ Initialize rop with a copy of the algorithm and state from op }
procedure mp_randinit_set(out rop: randstate_t; var op: randstate_t); cdecl; external LIB name '__gmp_randinit_set';
{ Set an initial seed value into state }
procedure mp_randseed(var state: randstate_t; var seed: mpz_t); cdecl; external LIB name '__gmp_randseed';
{ Set an initial seed value into state }
procedure mp_randseed_ui(var state: randstate_t; seed: valuint); cdecl; external LIB name '__gmp_randseed_ui';
{ Free all memory occupied by state }
procedure mp_randclear(var state: randstate_t); cdecl; external LIB name '__gmp_randclear';
{ Return an uniformly distributed random number of n bits, ie. in the range 0 to 2^n−1 inclusive }
function mp_urandomb_ui(var state: randstate_t; n: valuint): valuint; cdecl; external LIB name '__gmp_urandomb_ui';
{ Return an uniformly distributed random number in the range 0 to n − 1, inclusive }
function mp_urandomm_ui(var state: randstate_t; n: valuint): valuint; cdecl; external LIB name '__gmp_urandomm_ui';

// ---- Formatted Input/Output ----

{ Form a null-terminated string in a block of memory obtained from the current memory allocation function }
function mp_asprintf(out pp: pchar; fmt: pchar; args: array of const): longint; cdecl; external LIB name '__gmp_asprintf';
{ Form a null-terminated string in a block of memory obtained from the current memory allocation function }
function mp_asprintf(out pp: pchar; fmt: pchar): longint; cdecl; varargs; external LIB name '__gmp_asprintf';
{ Print to the standard output stdout. Return the number of characters written, or −1 if an error occurred. }
function mp_printf(fmt: pchar; args: array of const): longint; cdecl; external LIB name '__gmp_printf';
{ Print to the standard output stdout. Return the number of characters written, or −1 if an error occurred. }
function mp_printf(fmt: pchar): longint; cdecl; varargs; external LIB name '__gmp_printf';
{ Form a null-terminated string in buf. No more than size bytes will be written. }
function mp_snprintf(buf: pchar; size: sizeuint; fmt: pchar; args: array of const): longint; cdecl; external LIB name '__gmp_snprintf';
{ Form a null-terminated string in buf. No more than size bytes will be written. }
function mp_snprintf(buf: pchar; size: sizeuint; fmt: pchar): longint; cdecl; varargs; external LIB name '__gmp_snprintf';
{ Form a null-terminated string in buf. Return the number of characters written, excluding the terminating null. }
function mp_sprintf(buf, fmt: pchar; args: array of const): longint; cdecl; external LIB name '__gmp_sprintf';
{ Form a null-terminated string in buf. Return the number of characters written, excluding the terminating null. }
function mp_sprintf(buf, fmt: pchar): longint; cdecl; varargs; external LIB name '__gmp_sprintf';
{ Read from the standard input stdin }
function mp_scanf(fmt: pchar; args: array of const): longint; cdecl; external LIB name '__gmp_scanf';
{ Read from the standard input stdin }
function mp_scanf(fmt: pchar): longint; cdecl; varargs; external LIB name '__gmp_scanf';
{ Read from a null-terminated string s }
function mp_sscanf(s, fmt: pchar; args: array of const): longint; cdecl; external LIB name '__gmp_sscanf';
{ Read from a null-terminated string s }
function mp_sscanf(s, fmt: pchar): longint; cdecl; varargs; external LIB name '__gmp_sscanf';

// ---- integer Functions ----

{ Change the space for integer to new_alloc limbs }
function mpz_realloc(var integer_: mpz_t; new_alloc: mp_size_t): pointer; cdecl; external LIB name '__gmpz_realloc';
{ Set rop to the absolute value of op }
procedure mpz_abs(var rop, op: mpz_t); cdecl; external LIB name '__gmpz_abs';
{ Set rop to op1 + op2 }
procedure mpz_add(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_add';
{ Set rop to op1 + op2 }
procedure mpz_add_ui(var rop, op1: mpz_t; op2: valuint); cdecl; external LIB name '__gmpz_add_ui';
{ Set rop to rop + op1 × op2 }
procedure mpz_addmul(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_addmul';
{ Set rop to rop + op1 × op2 }
procedure mpz_addmul_ui(var rop, op1: mpz_t; op2: valuint); cdecl; external LIB name '__gmpz_addmul_ui';
{ Set rop to op1 bitwise-and op2 }
procedure mpz_and(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_and';
{ _Fixed_ space of fixed_num_bits is allocated to each of the array size integers in integer array }
procedure mpz_array_init(var integer_array: mpz_t; array_size, fixed_num_bits: mp_size_t); cdecl; external LIB name '__gmpz_array_init';
{ Compute the binomial coefficient (n over k) and store the result in rop }
procedure mpz_bin_ui(var rop, n: mpz_t; k: valuint); cdecl; external LIB name '__gmpz_bin_ui';
{ Compute the binomial coefficient (n over k) and store the result in rop }
procedure mpz_bin_uiui(var rop: mpz_t; n, k: valuint); cdecl; external LIB name '__gmpz_bin_uiui';
{ Divide n by d, forming a quotient q. Round mode ceil. }
procedure mpz_cdiv_q(var q, n, d: mpz_t); cdecl; external LIB name '__gmpz_cdiv_q';
{ Divide n by d, forming a quotient q. d = 2^b. Round mode ceil. }
procedure mpz_cdiv_q_2exp(var q, n: mpz_t; b: valuint); cdecl; external LIB name '__gmpz_cdiv_q_2exp';
{ Divide n by d, forming a quotient q. Round mode ceil. }
function mpz_cdiv_q_ui(var q, n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_cdiv_q_ui';
{ Divide n by d, forming a quotient q and remainder r. Round mode ceil. }
procedure mpz_cdiv_qr(var q, r, n, d: mpz_t); cdecl; external LIB name '__gmpz_cdiv_qr';
{ Divide n by d, forming a quotient q and remainder r. Round mode ceil. }
function mpz_cdiv_qr_ui(var q, r, n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_cdiv_qr_ui';
{ Divide n by d, forming a remainder r. Round mode ceil. }
procedure mpz_cdiv_r(var r, n, d: mpz_t); cdecl; external LIB name '__gmpz_cdiv_r';
{ Divide n by d, forming a remainder r. d = 2^b. Round mode ceil. }
procedure mpz_cdiv_r_2exp(var r, n: mpz_t; b: valuint); cdecl; external LIB name '__gmpz_cdiv_r_2exp';
{ Divide n by d, forming a remainder r. Round mode ceil. }
function mpz_cdiv_r_ui(var r, n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_cdiv_r_ui';
{ Divide n by d. Round mode ceil. }
function mpz_cdiv_ui(var n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_cdiv_ui';
{ Free the space occupied by integer. Call this function for all mpz_t variables when you are done with them. }
procedure mpz_clear(var integer_: mpz_t); cdecl; external LIB name '__gmpz_clear';
{ Clear bit bit_index in rop }
procedure mpz_clrbit(var rop: mpz_t; bit_index: valuint); cdecl; external LIB name '__gmpz_clrbit';
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative value if op1 < op2. }
function mpz_cmp(var op1, op2: mpz_t): longint; cdecl; external LIB name '__gmpz_cmp';
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative value if op1 < op2. }
function mpz_cmp_d(var op1: mpz_t; op2: double): longint; cdecl; external LIB name '__gmpz_cmp_d';
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative value if op1 < op2. }
function mpz_cmp_si(var op1: mpz_t; op2: valsint): longint; cdecl; external LIB name '__gmpz_cmp_si';
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative value if op1 < op2. }
function mpz_cmp_ui(var op1: mpz_t; op2: valuint): longint; cdecl; external LIB name '__gmpz_cmp_ui';
{ Compare the absolute values of op1 and op2. Return a positive value if |op1| > |op2|, zero if |op1| = |op2|, or a negative value if |op1| < |op2|. }
function mpz_cmpabs(var op1, op2: mpz_t): longint; cdecl; external LIB name '__gmpz_cmpabs';
{ Compare the absolute values of op1 and op2. Return a positive value if |op1| > |op2|, zero if |op1| = |op2|, or a negative value if |op1| < |op2|. }
function mpz_cmpabs_d(var op1: mpz_t; op2: double): longint; cdecl; external LIB name '__gmpz_cmpabs_d';
{ Compare the absolute values of op1 and op2. Return a positive value if |op1| > |op2|, zero if |op1| = |op2|, or a negative value if |op1| < |op2|. }
function mpz_cmpabs_ui(var op1: mpz_t; op2: valuint): longint; cdecl; external LIB name '__gmpz_cmpabs_ui';
{ Set rop to the one’s complement of op }
procedure mpz_com(var rop, op: mpz_t); cdecl; external LIB name '__gmpz_com';
{ Complement bit bit_index in rop }
procedure mpz_combit(var rop: mpz_t; bit_index: valuint); cdecl; external LIB name '__gmpz_combit';
{ Return non-zero if n is congruent to c modulo d }
function mpz_congruent_p(var n, c, d: mpz_t): longint; cdecl; external LIB name '__gmpz_congruent_p';
{ Return non-zero if n is congruent to c modulo 2^b }
function mpz_congruent_2exp_p(var n, c: mpz_t; b: valuint): longint; cdecl; external LIB name '__gmpz_congruent_2exp_p';
{ Return non-zero if n is congruent to c modulo d }
function mpz_congruent_ui_p(var n: mpz_t; c, d: valuint): longint; cdecl; external LIB name '__gmpz_congruent_ui_p';
{ Set q to n/d }
procedure mpz_divexact(var q, n, d: mpz_t); cdecl; external LIB name '__gmpz_divexact';
{ Set q to n/d }
procedure mpz_divexact_ui(var q, n: mpz_t; d: valuint); cdecl; external LIB name '__gmpz_divexact_ui';
{ Return non-zero if n is exactly divisible by d }
function mpz_divisible_p(var n, d: mpz_t): longint; cdecl; external LIB name '__gmpz_divisible_p';
{ Return non-zero if n is exactly divisible by d }
function mpz_divisible_ui_p(var n: mpz_t; d: valuint): longint; cdecl; external LIB name '__gmpz_divisible_ui_p';
{ Return non-zero if n is exactly divisible by by 2^b }
function mpz_divisible_2exp_p(var n: mpz_t; b: valuint): longint; cdecl; external LIB name '__gmpz_divisible_2exp_p';

// GDB only: procedure mpz_dump(var _para1: mpz_t); cdecl; external LIB name '__gmpz_dump';

{ Fill buf with word data from op }
function mpz_export(out buf; out countp: sizeuint; order: longint; size: sizeuint; endian: longint; nails: sizeuint; var op: mpz_t): pointer; cdecl; external LIB name '__gmpz_export';
{ Set rop to op!, the factorial of op }
procedure mpz_fac_ui(var rop: mpz_t; op: valuint); cdecl; external LIB name '__gmpz_fac_ui';
{ Divide n by d, forming a quotient q. Round mode floor. }
procedure mpz_fdiv_q(var q, n, d: mpz_t); cdecl; external LIB name '__gmpz_fdiv_q';
{ Divide n by d, forming a quotient q. d = 2^b. Round mode floor. }
procedure mpz_fdiv_q_2exp(var q, n: mpz_t; b: valuint); cdecl; external LIB name '__gmpz_fdiv_q_2exp';
{ Divide n by d, forming a quotient q. Round mode floor. }
function mpz_fdiv_q_ui(var q, n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_fdiv_q_ui';
{ Divide n by d, forming a quotient q and remainder r. Round mode floor. }
procedure mpz_fdiv_qr(var q, r, n, d: mpz_t); cdecl; external LIB name '__gmpz_fdiv_qr';
{ Divide n by d, forming a quotient q and remainder r. Round mode floor. }
function mpz_fdiv_qr_ui(var q, r, n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_fdiv_qr_ui';
{ Divide n by d, forming a remainder r. Round mode floor. }
procedure mpz_fdiv_r(var r, n, d: mpz_t); cdecl; external LIB name '__gmpz_fdiv_r';
{ Divide n by d, forming a remainder r. d = 2^b. Round mode floor. }
procedure mpz_fdiv_r_2exp(var r, n: mpz_t; b: valuint); cdecl; external LIB name '__gmpz_fdiv_r_2exp';
{ Divide n by d, forming a remainder r. Round mode floor. }
function mpz_fdiv_r_ui(var r, n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_fdiv_r_ui';
{ Divide n by d. Round mode floor. }
function mpz_fdiv_ui(var n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_fdiv_ui';
{ Set fn to to Fn, the n’th Fibonacci number }
procedure mpz_fib_ui(var fn: mpz_t; n: valuint); cdecl; external LIB name '__gmpz_fib_ui';
{ Set fn to Fn, and fnsub1 to Fn−1 }
procedure mpz_fib2_ui(var fn, fnsub1: mpz_t; n: valuint); cdecl; external LIB name '__gmpz_fib2_ui';
{ Return non-zero iff the value of op fits in an signed int. Otherwise, return zero. }
function mpz_fits_sint_p(var op: mpz_t): longint; cdecl; external LIB name '__gmpz_fits_sint_p';
{ Return non-zero iff the value of op fits in an signed long int. Otherwise, return zero. }
function mpz_fits_slong_p(var op: mpz_t): longint; cdecl; external LIB name '__gmpz_fits_slong_p';
{ Return non-zero iff the value of op fits in an signed short int. Otherwise, return zero. }
function mpz_fits_sshort_p(var op: mpz_t): longint; cdecl; external LIB name '__gmpz_fits_sshort_p';
{ Return non-zero iff the value of op fits in an unsigned int. Otherwise, return zero. }
function mpz_fits_uint_p(var op: mpz_t): longint; cdecl; external LIB name '__gmpz_fits_uint_p';
{ Return non-zero iff the value of op fits in an unsigned long int. Otherwise, return zero. }
function mpz_fits_ulong_p(var op: mpz_t): longint; cdecl; external LIB name '__gmpz_fits_ulong_p';
{ Return non-zero iff the value of op fits in an unsigned short int. Otherwise, return zero. }
function mpz_fits_ushort_p(var op: mpz_t): longint; cdecl; external LIB name '__gmpz_fits_ushort_p';
{ Set rop to the greatest common divisor of op1 and op2 }
procedure mpz_gcd(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_gcd';
{ Compute the greatest common divisor of op1 and op2. If rop is not NULL, store the result there. }
function mpz_gcd_ui(var rop, op1: mpz_t; op2: valuint): valuint; cdecl; external LIB name '__gmpz_gcd_ui';
{ Set g to the greatest common divisor of a and b, and in addition set s and t to coefficients satisfying as + bt = g }
procedure mpz_gcdext(var g, s, t, a, b: mpz_t); cdecl; external LIB name '__gmpz_gcdext';
{ Convert op to a double, truncating if necessary (ie. rounding towards zero) }
function mpz_get_d(var op: mpz_t): double; cdecl; external LIB name '__gmpz_get_d';
{ Convert op to a double, truncating if necessary (ie. rounding towards zero), and returning the exponent separately }
function mpz_get_d_2exp(out exp: valsint; var op: mpz_t): double; cdecl; external LIB name '__gmpz_get_d_2exp';
{ Return the value of op as a signed long }
function mpz_get_si(var op: mpz_t): valsint; cdecl; external LIB name '__gmpz_get_si';
{ Convert op to a string of digits in base base. The base argument may vary from 2 to 62 or from −2 to −36. If str is NULL, the result string is allocated using the current allocation function }
function mpz_get_str(str: pchar; base: longint; var op: mpz_t): pchar; cdecl; external LIB name '__gmpz_get_str';
{ Return the value of op as an unsigned long }
function mpz_get_ui(var op: mpz_t): valuint; cdecl; external LIB name '__gmpz_get_ui';
{ Return limb number n from op }
function mpz_getlimbn(var op: mpz_t; n: mp_size_t): mp_limb_t; cdecl; external LIB name '__gmpz_getlimbn';
{ If op1 and op2 are both >= 0 or both < 0, return the hamming distance between the two operands, which is the number of bit positions where op1 and op2 have different bit values }
function mpz_hamdist(var op1, op2: mpz_t): valuint; cdecl; external LIB name '__gmpz_hamdist';
{ Set rop from an array of word data at op }
procedure mpz_import(var rop: mpz_t; count: sizeuint; order: longint; size: sizeuint; endian: longint; nails: sizeuint; const op); cdecl; external LIB name '__gmpz_import';
{ Initialize integer, and set its value to 0 }
procedure mpz_init(out integer_: mpz_t); cdecl; external LIB name '__gmpz_init';
{ Initialize integer, with space for n bits, and set its value to 0 }
procedure mpz_init2(out integer_: mpz_t; n: valuint); cdecl; external LIB name '__gmpz_init2';
{ Initialize rop with limb space and set the initial numeric value from op }
procedure mpz_init_set(out rop: mpz_t; var op: mpz_t); cdecl; external LIB name '__gmpz_init_set';
{ Initialize rop with limb space and set the initial numeric value from op }
procedure mpz_init_set_d(out rop: mpz_t; op: double); cdecl; external LIB name '__gmpz_init_set_d';
{ Initialize rop with limb space and set the initial numeric value from op }
procedure mpz_init_set_si(out rop: mpz_t; op: valsint); cdecl; external LIB name '__gmpz_init_set_si';
{ Initialize rop and set its value like mpz_set_str }
function mpz_init_set_str(out rop: mpz_t; str: pchar; base: longint): longint; cdecl; external LIB name '__gmpz_init_set_str';
{ Initialize rop with limb space and set the initial numeric value from op }
procedure mpz_init_set_ui(out rop: mpz_t; op: valuint); cdecl; external LIB name '__gmpz_init_set_ui';
{ Compute the inverse of op1 modulo op2 and put the result in rop }
function mpz_invert(var rop, op1, op2: mpz_t): longint; cdecl; external LIB name '__gmpz_invert';
{ Set rop to op1 bitwise inclusive-or op2 }
procedure mpz_ior(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_ior';
{ Calculate the Jacobi symbol. This is defined only for b odd }
function mpz_jacobi(var a, b: mpz_t): longint; cdecl; external LIB name '__gmpz_jacobi';
{ Calculate the Jacobi symbol with the Kronecker extension }
function mpz_kronecker_si(var a: mpz_t; b: valsint): longint; cdecl; external LIB name '__gmpz_kronecker_si';
{ Calculate the Jacobi symbol with the Kronecker extension }
function mpz_kronecker_ui(var a: mpz_t; b: valuint): longint; cdecl; external LIB name '__gmpz_kronecker_ui';
{ Calculate the Jacobi symbol with the Kronecker extension }
function mpz_si_kronecker(a: valsint; var b: mpz_t): longint; cdecl; external LIB name '__gmpz_si_kronecker';
{ Calculate the Jacobi symbol with the Kronecker extension }
function mpz_ui_kronecker(a: valuint; var b: mpz_t): longint; cdecl; external LIB name '__gmpz_ui_kronecker';
{ Set rop to the least common multiple of op1 and op2 }
procedure mpz_lcm(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_lcm';
{ Set rop to the least common multiple of op1 and op2 }
procedure mpz_lcm_ui(var rop, op1: mpz_t; op2: valuint); cdecl; external LIB name '__gmpz_lcm_ui';
{ Set ln to to Ln, the n’th Lucas number }
procedure mpz_lucnum_ui(var ln: mpz_t; n: valuint); cdecl; external LIB name '__gmpz_lucnum_ui';
{ Set ln to Ln, and lnsub1 to Ln−1 }
procedure mpz_lucnum2_ui(var ln, lnsub1: mpz_t; n: valuint); cdecl; external LIB name '__gmpz_lucnum2_ui';

// No docs: function mpz_millerrabin(var _para1: mpz_t; _para2: longint): longint; cdecl; external LIB name '__gmpz_millerrabin';

{ Set r to n mod d. The sign of the divisor is ignored; the result is always non-negative. }
procedure mpz_mod(var r, n, d: mpz_t); cdecl; external LIB name '__gmpz_mod';
{ Set rop to op1 × op2 }
procedure mpz_mul(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_mul';
{ Set rop to op1 × 2^op2. This operation can also be defined as a left shift by op2 bits. }
procedure mpz_mul_2exp(var rop, op1: mpz_t; op2: valuint); cdecl; external LIB name '__gmpz_mul_2exp';
{ Set rop to op1 × op2 }
procedure mpz_mul_si(var rop, op1: mpz_t; op2: valsint); cdecl; external LIB name '__gmpz_mul_si';
{ Set rop to op1 × op2 }
procedure mpz_mul_ui(var rop, op1: mpz_t; op2: valuint); cdecl; external LIB name '__gmpz_mul_ui';
{ Set rop to −op }
procedure mpz_neg(var rop, op: mpz_t); cdecl; external LIB name '__gmpz_neg';
{ Set rop to the next prime greater than op }
procedure mpz_nextprime(var rop, op: mpz_t); cdecl; external LIB name '__gmpz_nextprime';
{ Return non-zero if op is a perfect power, i.e., if there exist integers a and b, with b > 1, such that op = a^b }
function mpz_perfect_power_p(var op: mpz_t): longint; cdecl; external LIB name '__gmpz_perfect_power_p';
{ Return non-zero if op is a perfect square, i.e., if the square root of op is an integer }
function mpz_perfect_square_p(var op: mpz_t): longint; cdecl; external LIB name '__gmpz_perfect_square_p';
{ If op >= 0, return the population count of op, which is the number of 1 bits in the binary representation }
function mpz_popcount(var op: mpz_t): valuint; cdecl; external LIB name '__gmpz_popcount';
{ Set rop to base^exp. The case 0^0 yields 1. }
procedure mpz_pow_ui(var rop, base: mpz_t; exp: valuint); cdecl; external LIB name '__gmpz_pow_ui';
{ Set rop to base^exp mod mod_ }
procedure mpz_powm(var rop, base, exp, mod_: mpz_t); cdecl; external LIB name '__gmpz_powm';
{ Set rop to base^exp mod mod_ }
procedure mpz_powm_ui(var rop, base: mpz_t; exp: valuint; var mod_: mpz_t); cdecl; external LIB name '__gmpz_powm_ui';
{ Determine whether n is prime. Return 2 if n is definitely prime, return 1 if n is probably prime (without being certain), or return 0 if n is definitely composite. }
function mpz_probab_prime_p(var n: mpz_t; reps: longint): longint; cdecl; external LIB name '__gmpz_probab_prime_p';

{ Obsolete: Generate a random integer of at most max_size limbs }
// procedure mpz_random(var rop: mpz_t; max_size: mp_size_t); cdecl; external LIB name '__gmpz_random';
{ Obsolete: Generate a random integer of at most max_size limbs, with long strings of zeros and ones in the binary representation }
// procedure mpz_random2(var rop: mpz_t; max_size: mp_size_t); cdecl; external LIB name '__gmpz_random2';

{ Change the space allocated for integer to n bits. The value in integer is preserved if it fits, or is set to 0 if not. }
procedure mpz_realloc2(var integer_: mpz_t; n: valuint); cdecl; external LIB name '__gmpz_realloc2';
{ Remove all occurrences of the factor f from op and store the result in rop }
function mpz_remove(var rop, op, f: mpz_t): valuint; cdecl; external LIB name '__gmpz_remove';
{ Set rop to trunc(op^(1/n)), the truncated integer part of the nth root of op. Return non-zero if the computation was exact, i.e., if op is rop to the nth power. }
function mpz_root(var rop, op: mpz_t; n: valuint): longint; cdecl; external LIB name '__gmpz_root';
{ Set root to trunc(u^(1/n)), the truncated integer part of the nth root of u. Set rem to the remainder, (u − root^n). }
procedure mpz_rootrem(var root, rem, u: mpz_t; n: valuint); cdecl; external LIB name '__gmpz_rootrem';
{ Generate a random integer with long strings of zeros and ones in the binary representation }
procedure mpz_rrandomb(var rop: mpz_t; var state: randstate_t; n: valuint); cdecl; external LIB name '__gmpz_rrandomb';
{ Scan op, starting from bit starting_bit, towards more significant bits, until the first 0 bit is found }
function mpz_scan0(var op: mpz_t; starting_bit: valuint): valuint; cdecl; external LIB name '__gmpz_scan0';
{ Scan op, starting from bit starting_bit, towards more significant bits, until the first 1 bit is found }
function mpz_scan1(var op: mpz_t; starting_bit: valuint): valuint; cdecl; external LIB name '__gmpz_scan1';
{ Set the value of rop from op }
procedure mpz_set(var rop, op: mpz_t); cdecl; external LIB name '__gmpz_set';
{ Set the value of rop from op }
procedure mpz_set_d(var rop: mpz_t; op: double); cdecl; external LIB name '__gmpz_set_d';
{ Set the value of rop from op }
procedure mpz_set_f(var rop: mpz_t; var op: mpf_t); cdecl; external LIB name '__gmpz_set_f';
{ Set the value of rop from op }
procedure mpz_set_q(var rop: mpz_t; var op: mpq_t); cdecl; external LIB name '__gmpz_set_q';
{ Set the value of rop from op }
procedure mpz_set_si(var rop: mpz_t; op: valsint); cdecl; external LIB name '__gmpz_set_si';
{ Set the value of rop from str, a null-terminated C string in base base. White space is allowed in the string, and is simply ignored. }
function mpz_set_str(var rop: mpz_t; str: pchar; base: longint): longint; cdecl; external LIB name '__gmpz_set_str';
{ Set the value of rop from op }
procedure mpz_set_ui(var rop: mpz_t; op: valuint); cdecl; external LIB name '__gmpz_set_ui';
{ Set bit bit_index in rop }
procedure mpz_setbit(var rop: mpz_t; bit_index: valuint); cdecl; external LIB name '__gmpz_setbit';
{ Return the size of op measured in number of limbs }
function mpz_size(var op: mpz_t): sizeuint; cdecl; external LIB name '__gmpz_size';
{ Return the size of op measured in number of digits in the given base }
function mpz_sizeinbase(var op: mpz_t; base: longint): sizeuint; cdecl; external LIB name '__gmpz_sizeinbase';
{ Set rop to trunc(sqrt(op)), the truncated integer part of the square root of op }
procedure mpz_sqrt(var rop, op: mpz_t); cdecl; external LIB name '__gmpz_sqrt';
{ Set rop1 to trunc(sqrt(op)), likempz_sqrt. Set rop2 to the remainder (op − rop1^2), which will be zero if op is a perfect square. }
procedure mpz_sqrtrem(var rop1, rop2, op: mpz_t); cdecl; external LIB name '__gmpz_sqrtrem';
{ Set rop to op1 − op2 }
procedure mpz_sub(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_sub';
{ Set rop to op1 − op2 }
procedure mpz_sub_ui(var rop, op1: mpz_t; op2: valuint); cdecl; external LIB name '__gmpz_sub_ui';
{ Set rop to op1 − op2 }
procedure mpz_ui_sub(var rop: mpz_t; op1: valuint; var op2: mpz_t); cdecl; external LIB name '__gmpz_ui_sub';
{ Set rop to rop − op1 × op2 }
procedure mpz_submul(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_submul';
{ Set rop to rop − op1 × op2 }
procedure mpz_submul_ui(var rop, op1: mpz_t; op2: valuint); cdecl; external LIB name '__gmpz_submul_ui';
{ Swap the values rop1 and rop2 efficiently }
procedure mpz_swap(var rop1, rop2: mpz_t); cdecl; external LIB name '__gmpz_swap';
{ Divide n by d, forming a quotient q. Round mode trunc. }
procedure mpz_tdiv_q(var q, n, d: mpz_t); cdecl; external LIB name '__gmpz_tdiv_q';
{ Divide n by d, forming a quotient q. d = 2^b. Round mode trunc. }
procedure mpz_tdiv_q_2exp(var q, n: mpz_t; b: valuint); cdecl; external LIB name '__gmpz_tdiv_q_2exp';
{ Divide n by d, forming a quotient q. Round mode trunc. }
function mpz_tdiv_q_ui(var q, n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_tdiv_q_ui';
{ Divide n by d, forming a quotient q and remainder r. Round mode trunc. }
procedure mpz_tdiv_qr(var q, r, n, d: mpz_t); cdecl; external LIB name '__gmpz_tdiv_qr';
{ Divide n by d, forming a quotient q and remainder r. Round mode trunc. }
function mpz_tdiv_qr_ui(var q, r, n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_tdiv_qr_ui';
{ Divide n by d, forming a remainder r. Round mode trunc. }
procedure mpz_tdiv_r(var r, n, d: mpz_t); cdecl; external LIB name '__gmpz_tdiv_r';
{ Divide n by d, forming a remainder r. d = 2^b. Round mode trunc. }
procedure mpz_tdiv_r_2exp(var r, n: mpz_t; b: valuint); cdecl; external LIB name '__gmpz_tdiv_r_2exp';
{ Divide n by d, forming a remainder r. Round mode trunc. }
function mpz_tdiv_r_ui(var r, n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_tdiv_r_ui';
{ Divide n by d. Round mode trunc. }
function mpz_tdiv_ui(var n: mpz_t; d: valuint): valuint; cdecl; external LIB name '__gmpz_tdiv_ui';
{ Test bit bit_index in op and return 0 or 1 accordingly }
function mpz_tstbit(var rop: mpz_t; bit_index: valuint): longint; cdecl; external LIB name '__gmpz_tstbit';
{ Set rop to base^exp. The case 0^0 yields 1 }
procedure mpz_ui_pow_ui(var rop: mpz_t; base, exp: valuint); cdecl; external LIB name '__gmpz_ui_pow_ui';
{ Generate a uniformly distributed random integer in the range 0 to 2^n − 1, inclusive }
procedure mpz_urandomb(var rop: mpz_t; var state: randstate_t; n: valuint); cdecl; external LIB name '__gmpz_urandomb';
{ Generate a uniform random integer in the range 0 to n − 1, inclusive }
procedure mpz_urandomm(var rop: mpz_t; var state: randstate_t; var n: mpz_t); cdecl; external LIB name '__gmpz_urandomm';
{ Set rop to op1 bitwise exclusive-or op2 }
procedure mpz_xor(var rop, op1, op2: mpz_t); cdecl; external LIB name '__gmpz_xor';

// ---- Rational Number Functions ----

{ Set rop to the absolute value of op }
procedure mpq_abs(var rop, op: mpq_t); cdecl; external LIB name '__gmpq_abs';
{ Set sum to addend1 + addend2 }
procedure mpq_add(var sum, addend1, addend2: mpq_t); cdecl; external LIB name '__gmpq_add';
{ Remove any factors that are common to the numerator and denominator of op, and make the denominator positive }
procedure mpq_canonicalize(var op: mpq_t); cdecl; external LIB name '__gmpq_canonicalize';
{ Free the space occupied by rational number }
procedure mpq_clear(var rational_number: mpq_t); cdecl; external LIB name '__gmpq_clear';
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2 }
function mpq_cmp(var op1, op2: mpq_t): longint; cdecl; external LIB name '__gmpq_cmp';
{ Compare op1 and num2/den2. Return a positive value if op1 > num2/den2, zero if op1 = num2/den2, and a negative value if op1 < num2/den2 }
function mpq_cmp_si(var op1: mpq_t; num2: valsint; den2: valuint): longint; cdecl; external LIB name '__gmpq_cmp_si';
{ Compare op1 and num2/den2. Return a positive value if op1 > num2/den2, zero if op1 = num2/den2, and a negative value if op1 < num2/den2 }
function mpq_cmp_ui(var op1: mpq_t; num2, den2: valuint): longint; cdecl; external LIB name '__gmpq_cmp_ui';
{ Set quotient to dividend/divisor }
procedure mpq_div(var quotient, dividend, divisor: mpq_t); cdecl; external LIB name '__gmpq_div';
{ Set rop to op1/(2^op2) }
procedure mpq_div_2exp(var rop, op1: mpq_t; op2: valuint); cdecl; external LIB name '__gmpq_div_2exp';
{ Return non-zero if op1 and op2 are equal, zero if they are non-equal }
function mpq_equal(var op1, op2: mpq_t): longint; cdecl; external LIB name '__gmpq_equal';
{ Get the numerator of a rational }
procedure mpq_get_num(var numerator: mpz_t; var rational: mpq_t); cdecl; external LIB name '__gmpq_get_num';
{ Get the denominator of a rational }
procedure mpq_get_den(var denominator: mpz_t; var rational: mpq_t); cdecl; external LIB name '__gmpq_get_den';
{ Convert op to a double, truncating if necessary (ie. rounding towards zero) }
function mpq_get_d(var op: mpq_t): double; cdecl; external LIB name '__gmpq_get_d';
{ Convert op to a string of digits in base base }
function mpq_get_str(str: pchar; base: longint; var op: mpq_t): pchar; cdecl; external LIB name '__gmpq_get_str';
{ Initialize dest rational and set it to 0/1 }
procedure mpq_init(out dest_rational: mpq_t); cdecl; external LIB name '__gmpq_init';
{ Set inverted_number to 1/number }
procedure mpq_inv(var inverted_number, number: mpq_t); cdecl; external LIB name '__gmpq_inv';
{ Set product to multiplier × multiplicand }
procedure mpq_mul(var product, multiplier, multiplicand: mpq_t); cdecl; external LIB name '__gmpq_mul';
{ Set rop to op1 × (2^op2) }
procedure mpq_mul_2exp(var rop, op1: mpq_t; op2: valuint); cdecl; external LIB name '__gmpq_mul_2exp';
{ Set negated_operand to −operand }
procedure mpq_neg(var negated_operand, operand: mpq_t); cdecl; external LIB name '__gmpq_neg';
{ Assign rop from op }
procedure mpq_set(var rop, op: mpq_t); cdecl; external LIB name '__gmpq_set';
{ Set rop to the value of op. There is no rounding, this conversion is exact. }
procedure mpq_set_d(var rop: mpq_t; op: double); cdecl; external LIB name '__gmpq_set_d';
{ Set the denominator of a rational }
procedure mpq_set_den(var rational: mpq_t; var denominator: mpz_t); cdecl; external LIB name '__gmpq_set_den';
{ Set rop to the value of op. There is no rounding, this conversion is exact. }
procedure mpq_set_f(var rop: mpq_t; var op: mpf_t); cdecl; external LIB name '__gmpq_set_f';
{ Set the numerator of a rational }
procedure mpq_set_num(var rational: mpq_t; var numerator: mpz_t); cdecl; external LIB name '__gmpq_set_num';
{ Set the value of rop to op1/op2 }
procedure mpq_set_si(var rop: mpq_t; op1: valsint; op2: valuint); cdecl; external LIB name '__gmpq_set_si';
{ Set rop from a null-terminated string str in the given base }
function mpq_set_str(var rop: mpq_t; str: pchar; base: longint): longint; cdecl; external LIB name '__gmpq_set_str';
{ Set the value of rop to op1/op2 }
procedure mpq_set_ui(var rop: mpq_t; op1, op2: valuint); cdecl; external LIB name '__gmpq_set_ui';
{ Assign rop from op }
procedure mpq_set_z(var rop: mpq_t; var op: mpz_t); cdecl; external LIB name '__gmpq_set_z';
{ Set difference to minuend − subtrahend }
procedure mpq_sub(var difference, minuend, subtrahend: mpq_t); cdecl; external LIB name '__gmpq_sub';
{ Swap the values rop1 and rop2 efficiently }
procedure mpq_swap(var rop1, rop2: mpq_t); cdecl; external LIB name '__gmpq_swap';

// ---- Floating-point Functions ----

{ Set rop to the absolute value of op }
procedure mpf_abs(var rop, op: mpf_t); cdecl; external LIB name '__gmpf_abs';
{ Set rop to op1 + op2 }
procedure mpf_add(var rop, op1, op2: mpf_t); cdecl; external LIB name '__gmpf_add';
{ Set rop to op1 + op2 }
procedure mpf_add_ui(var rop, op1: mpf_t; op2: valuint); cdecl; external LIB name '__gmpf_add_ui';
{ Set rop to op rounded to the next higher integer }
procedure mpf_ceil(var rop, op: mpf_t); cdecl; external LIB name '__gmpf_ceil';
{ Free the space occupied by x }
procedure mpf_clear(var x: mpf_t); cdecl; external LIB name '__gmpf_clear';
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. }
function mpf_cmp(var op1, op2: mpf_t): longint; cdecl; external LIB name '__gmpf_cmp';
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. }
function mpf_cmp_d(var op1: mpf_t; op2: double): longint; cdecl; external LIB name '__gmpf_cmp_d';
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. }
function mpf_cmp_si(var op1: mpf_t; op2: valsint): longint; cdecl; external LIB name '__gmpf_cmp_si';
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. }
function mpf_cmp_ui(var op1: mpf_t; op2: valuint): longint; cdecl; external LIB name '__gmpf_cmp_ui';
{ Set rop to op1/op2 }
procedure mpf_div(var rop, op1, op2: mpf_t); cdecl; external LIB name '__gmpf_div';
{ Set rop to op1/(2^op2) }
procedure mpf_div_2exp(var rop, op1: mpf_t; op2: valuint); cdecl; external LIB name '__gmpf_div_2exp';
{ Set rop to op1/op2 }
procedure mpf_div_ui(var rop, op1: mpf_t; op2: valuint); cdecl; external LIB name '__gmpf_div_ui';

// GDB only: procedure mpf_dump(var _para1: mpf_t); cdecl; external LIB name '__gmpf_dump';

{ Return non-zero if the first op3 bits of op1 and op2 are equal, zero otherwise }
function mpf_eq(var op1, op2: mpf_t; op3: valuint): longint; cdecl; external LIB name '__gmpf_eq';
{ Return non-zero if op would fit in the respective C data type, when truncated to an integer }
function mpf_fits_sint_p(var op: mpf_t): longint; cdecl; external LIB name '__gmpf_fits_sint_p';
{ Return non-zero if op would fit in the respective C data type, when truncated to an integer }
function mpf_fits_slong_p(var op: mpf_t): longint; cdecl; external LIB name '__gmpf_fits_slong_p';
{ Return non-zero if op would fit in the respective C data type, when truncated to an integer }
function mpf_fits_sshort_p(var op: mpf_t): longint; cdecl; external LIB name '__gmpf_fits_sshort_p';
{ Return non-zero if op would fit in the respective C data type, when truncated to an integer }
function mpf_fits_uint_p(var op: mpf_t): longint; cdecl; external LIB name '__gmpf_fits_uint_p';
{ Return non-zero if op would fit in the respective C data type, when truncated to an integer }
function mpf_fits_ulong_p(var op: mpf_t): longint; cdecl; external LIB name '__gmpf_fits_ulong_p';
{ Return non-zero if op would fit in the respective C data type, when truncated to an integer }
function mpf_fits_ushort_p(var op: mpf_t): longint; cdecl; external LIB name '__gmpf_fits_ushort_p';
{ Set rop to op rounded to the next lower }
procedure mpf_floor(var rop, op: mpf_t); cdecl; external LIB name '__gmpf_floor';
{ Convert op to a double, truncating if necessary (ie. rounding towards zero) }
function mpf_get_d(var op: mpf_t): double; cdecl; external LIB name '__gmpf_get_d';
{ Convert op to a double, truncating if necessary (ie. rounding towards zero), and with an exponent returned separately }
function mpf_get_d_2exp(out exp: valsint; var op: mpf_t): double; cdecl; external LIB name '__gmpf_get_d_2exp';
{ Return the default precision actually used }
function mpf_get_default_prec: valuint; cdecl; external LIB name '__gmpf_get_default_prec';
{ Return the current precision of op, in bits }
function mpf_get_prec(var op: mpf_t): valuint; cdecl; external LIB name '__gmpf_get_prec';
{ Convert op to a long, truncating any fraction part }
function mpf_get_si(var op: mpf_t): valsint; cdecl; external LIB name '__gmpf_get_si';
{ Convert op to a string of digits in base base }
function mpf_get_str(str: pchar; out exp: mp_exp_t; base: longint; ndigits: sizeuint; var op: mpf_t): pchar; cdecl; external LIB name '__gmpf_get_str';
{ Convert op to a unsigned long, truncating any fraction part }
function mpf_get_ui(var op: mpf_t): valuint; cdecl; external LIB name '__gmpf_get_ui';
{ Initialize x to 0 }
procedure mpf_init(out x: mpf_t); cdecl; external LIB name '__gmpf_init';
{ Initialize x to 0 and set its precision to be at least prec bits }
procedure mpf_init2(out x: mpf_t; prec: valuint); cdecl; external LIB name '__gmpf_init2';
{ Initialize rop and set its value from op }
procedure mpf_init_set(out rop: mpf_t; var op: mpf_t); cdecl; external LIB name '__gmpf_init_set';
{ Initialize rop and set its value from op }
procedure mpf_init_set_d(out rop: mpf_t; op: double); cdecl; external LIB name '__gmpf_init_set_d';
{ Initialize rop and set its value from op }
procedure mpf_init_set_si(out rop: mpf_t; op: valsint); cdecl; external LIB name '__gmpf_init_set_si';
{ Initialize rop and set its value from the string in str }
function mpf_init_set_str(out rop: mpf_t; str: pchar; base: longint): longint; cdecl; external LIB name '__gmpf_init_set_str';
{ Initialize rop and set its value from op }
procedure mpf_init_set_ui(out rop: mpf_t; op: valuint); cdecl; external LIB name '__gmpf_init_set_ui';
{ Return non-zero if op is an integer }
function mpf_integer_p(var op: mpf_t): longint; cdecl; external LIB name '__gmpf_integer_p';
{ Set rop to op1 × op2 }
procedure mpf_mul(var rop, op1, op2: mpf_t); cdecl; external LIB name '__gmpf_mul';
{ Set rop to op1 × (2^op2) }
procedure mpf_mul_2exp(var rop, op1: mpf_t; op2: valuint); cdecl; external LIB name '__gmpf_mul_2exp';
{ Set rop to op1 × op2 }
procedure mpf_mul_ui(var rop, op1: mpf_t; op2: valuint); cdecl; external LIB name '__gmpf_mul_ui';
{ Set rop to −op }
procedure mpf_neg(var rop, op: mpf_t); cdecl; external LIB name '__gmpf_neg';
{ Set rop to op1^op2 }
procedure mpf_pow_ui(var rop, op1: mpf_t; op2: valuint); cdecl; external LIB name '__gmpf_pow_ui';
{ Generate a random float of at most max_size limbs, with long strings of zeros and ones in the binary representation }
procedure mpf_random2(var rop: mpf_t; max_size: mp_size_t; exp: mp_exp_t); cdecl; external LIB name '__gmpf_random2';
{ Compute the relative difference between op1 and op2 and store the result in rop }
procedure mpf_reldiff(var rop, op1, op2: mpf_t); cdecl; external LIB name '__gmpf_reldiff';
{ Set the value of rop from op }
procedure mpf_set(var rop, op: mpf_t); cdecl; external LIB name '__gmpf_set';
{ Set the value of rop from op }
procedure mpf_set_d(var rop: mpf_t; op: double); cdecl; external LIB name '__gmpf_set_d';
{ Set the default precision to be at least prec bits }
procedure mpf_set_default_prec(prec: valuint); cdecl; external LIB name '__gmpf_set_default_prec';
{ Set the precision of rop to be at least prec bits }
procedure mpf_set_prec(var rop: mpf_t; prec: valuint); cdecl; external LIB name '__gmpf_set_prec';
{ Set the precision of rop to be at least prec bits, without changing the memory allocated }
procedure mpf_set_prec_raw(var rop: mpf_t; prec: valuint); cdecl; external LIB name '__gmpf_set_prec_raw';
{ Set the value of rop from op }
procedure mpf_set_q(var rop: mpf_t; var op: mpq_t); cdecl; external LIB name '__gmpf_set_q';
{ Set the value of rop from op }
procedure mpf_set_si(var rop: mpf_t; op: valsint); cdecl; external LIB name '__gmpf_set_si';
{ Set the value of rop from the string in str }
function mpf_set_str(var rop: mpf_t; str: pchar; base: longint): longint; cdecl; external LIB name '__gmpf_set_str';
{ Set the value of rop from op }
procedure mpf_set_ui(var rop: mpf_t; op: valuint); cdecl; external LIB name '__gmpf_set_ui';
{ Set the value of rop from op }
procedure mpf_set_z(var rop: mpf_t; var op: mpz_t); cdecl; external LIB name '__gmpf_set_z';

// No docs: function mpf_size(var _para1: mpf_t): size_t; cdecl; external LIB name '__gmpf_size';

{ Set rop to op^(1/2) }
procedure mpf_sqrt(var rop, op: mpf_t); cdecl; external LIB name '__gmpf_sqrt';
{ Set rop to op^(1/2) }
procedure mpf_sqrt_ui(var rop: mpf_t; op: valuint); cdecl; external LIB name '__gmpf_sqrt_ui';
{ Set rop to op1 − op2 }
procedure mpf_sub(var rop, op1, op2: mpf_t); cdecl; external LIB name '__gmpf_sub';
{ Set rop to op1 − op2 }
procedure mpf_sub_ui(var rop, op1: mpf_t; op2: valuint); cdecl; external LIB name '__gmpf_sub_ui';
{ Swap rop1 and rop2 efficiently }
procedure mpf_swap(var rop1, rop2: mpf_t); cdecl; external LIB name '__gmpf_swap';
{ Set rop to op rounded to the integer towards zero }
procedure mpf_trunc(var rop, op: mpf_t); cdecl; external LIB name '__gmpf_trunc';
{ Set rop to op1/op2 }
procedure mpf_ui_div(var rop: mpf_t; op1: valuint; var op2: mpf_t); cdecl; external LIB name '__gmpf_ui_div';
{ Set rop to op1 − op2 }
procedure mpf_ui_sub(var rop: mpf_t; op1: valuint; var op2: mpf_t); cdecl; external LIB name '__gmpf_ui_sub';
{ Generate a uniformly distributed random float in rop, such that 0 <= rop < 1, with nbits significant bits in the mantissa }
procedure mpf_urandomb(var rop: mpf_t; var state: randstate_t; nbits: valuint); cdecl; external LIB name '__gmpf_urandomb';

// ---- Low-level Functions ----

{ Add [s1p, s1n] and [s2p, s2n], and write the s1n least significant limbs of the result to rp. Return carry, either 0 or 1. }
function mpn_add(rop, s1p: mpn_ptr; s1n: mp_size_t; s2p: mpn_ptr; s2n: mp_size_t): mp_limb_t; cdecl; external LIB name '__gmpn_add';
{ Add [s1p, n] and s2limb, and write the n least significant limbs of the result to rp. Return carry, either 0 or 1. }
function mpn_add_1(rp, s1p: mpn_ptr; n: mp_size_t; s2limb: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_add_1';
{ Add [s1p, n] and [s2p, n], and write the n least significant limbs of the result to rp. Return carry, either 0 or 1. }
function mpn_add_n(rop, s1p, s2p: mpn_ptr; n: mp_size_t): mp_limb_t; cdecl; external LIB name '__gmpn_add_n';
{ Multiply [s1p, n] and s2limb, and add the n least significant limbs of the product to [rp, n] and write the result to rp. Return the most significant limb of the product, plus carry-out from the addition. }
function mpn_addmul_1(rp, s1p: mpn_ptr; n: mp_size_t; s2limb: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_addmul_1';

{ Preliminary: This function puts the low floor(d/mp_bits_per_limb) limbs of q = [s1p, s1n]/[s2p, s2n] mod 2^d at rp, and returns the high d mod mp_bits_per_limb bits of q }
// function mpn_bdivmod(rp, s1p: mpn_t; s1n: mp_size_t; s2p: mpn_t; s2n: mp_size_t; d: valuint): mp_limb_t; cdecl; external LIB name '__gmpn_bdivmod';

{ Compare [s1p, n] and [s2p, n] and return a positive value if s1 > s2, 0 if they are equal, or a negative value if s1 < s2 }
function mpn_cmp(s1p, s2p: mpn_ptr; n: mp_size_t): longint; cdecl; external LIB name '__gmpn_cmp';
{ Divide [sp, n] by 3, expecting it to divide exactly, and writing the result to [rp, n] }
function mpn_divexact_by3c(rp, sp: mpn_ptr; n: mp_size_t; carry: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_divexact_by3c';

{ Obsolete: Divide [rs2p, rs2n] by [s3p, s3n], and write the quotient at r1p, with the exception of the most significant limb, which is returned }
// function mpn_divrem(r1p: mpn_t; qxn: mp_size_t; rs2p: mpn_t; rs2n: mp_size_t; s3p: mpn_t; s3n: mp_size_t): mp_limb_t; cdecl; external LIB name '__gmpn_divrem';

{ Divide [s2p, s2n] by s3limb, and write the quotient at r1p. Return the remainder }
function mpn_divrem_1(r1p: mpn_ptr; qxn: mp_size_t; s2p: mpn_ptr; s2n: mp_size_t; s2limb: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_divrem_1';

// No docs: function mpn_divrem_2(_para1: mpn_t; _para2: mp_size_t; _para3: mpn_t; _para4: mp_size_t; _para5: mpn_t): mp_limb_t; cdecl; external LIB name '__gmpn_divrem_2';

{ Set [rp, retval] to the greatest common divisor of [s1p, s1n] and [s2p, s2n] }
function mpn_gcd(rp, s1p: mpn_ptr; s1n: mp_size_t; s2p: mpn_ptr; s2n: mp_size_t): mp_size_t; cdecl; external LIB name '__gmpn_gcd';
{ Return the greatest common divisor of [s1p, s1n] and s2limb }
function mpn_gcd_1(s1p: mpn_ptr; s1n: mp_size_t; s2limb: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_gcd_1';
{ Calculate the greatest common divisor of [s1p, s1n] and [s2p, s2n] }
function mpn_gcdext(r1p, r2p: mpn_ptr; out r2n: mp_size_t; s1p: mpn_ptr; s1n: mp_size_t; s2p: mpn_ptr; s2n: mp_size_t): mp_size_t; cdecl; external LIB name '__gmpn_gcdext';
{ Convert [s1p, s1n] to a raw unsigned char array at str in base base, and return the number of characters produced }
function mpn_get_str(str: pbyte; base: longint; s1p: mpn_ptr; s1n: mp_size_t):sizeuint; cdecl; external LIB name '__gmpn_get_str';
{ Compute the hamming distance between [s1p, n] and [s2p, n], which is the number of bit positions where the two operands have different bit values }
function mpn_hamdist(s1p, s2p: mpn_ptr; n: mp_size_t): valuint; cdecl; external LIB name '__gmpn_hamdist';
{ Shift [sp, n] left by count bits, and write the result to [rp, n] }
function mpn_lshift(rp, sp: mpn_ptr; n: mp_size_t; count: dword): mp_limb_t; cdecl; external LIB name '__gmpn_lshift';
{ Divide [s1p, s1n] by s2limb, and return the remainder. s1n can be zero. }
function mpn_mod_1(s1p: mpn_ptr; s1n: mp_size_t; s2limb: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_mod_1';
{ Multiply [s1p, s1n] and [s2p, s2n], and write the result to rp. Return the most significant limb of the result. }
function mpn_mul(rp, s1p: mpn_ptr; s1n: mp_size_t; s2p: mpn_ptr; s2n: mp_size_t): mp_limb_t; cdecl; external LIB name '__gmpn_mul';
{ Multiply [s1p, n] by s2limb, and write the n least significant limbs of the product to rp. Return the most significant limb of the product. }
function mpn_mul_1(rp, s1p: mpn_ptr; n: mp_size_t; s2limb: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_mul_1';
{ Multiply [s1p, n] and [s2p, n], and write the 2*n-limb result to rp }
procedure mpn_mul_n(rp, s1p, s2p: mpn_ptr; n: mp_size_t); cdecl; external LIB name '__gmpn_mul_n';
{ Return non-zero iff [s1p, n] is a perfect square }
function mpn_perfect_square_p(s1p: mpn_ptr; n: mp_size_t): longint; cdecl; external LIB name '__gmpn_perfect_square_p';
{ Count the number of set bits in [s1p, n] }
function mpn_popcount(s1p: mpn_ptr; n: mp_size_t): valuint; cdecl; external LIB name '__gmpn_popcount';

// No docs: function mpn_pow_1(_para1, _para2: mpn_t; _para3: mp_size_t; _para4, _para5: mpn_t): mp_size_t; cdecl; external LIB name '__gmpn_pow_1';
// No docs: function mpn_preinv_mod_1(_para1: mpn_t; _para2: mp_size_t; _para3, _para4: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_preinv_mod_1';

{ Generate a random number of length r1n and store it at r1p }
procedure mpn_random(r1p: mpn_ptr; r1n: mp_size_t); cdecl; external LIB name '__gmpn_random';
{ Generate a random number of length r1n and store it at r1p }
procedure mpn_random2(r1p: mpn_ptr; r1n: mp_size_t); cdecl; external LIB name '__gmpn_random2';
{ Shift [sp, n] right by count bits, and write the result to [rp, n] }
function mpn_rshift(rp, sp: mpn_ptr; n: mp_size_t; count: dword): mp_limb_t; cdecl; external LIB name '__gmpn_rshift';
{ Scan s1p from bit position bit for the next clear bit }
function mpn_scan0(s1p: mpn_ptr; bit: valuint): valuint; cdecl; external LIB name '__gmpn_scan0';
{ Scan s1p from bit position bit for the next set bit }
function mpn_scan1(s1p: mpn_ptr; bit: valuint): valuint; cdecl; external LIB name '__gmpn_scan1';
{ Convert bytes [str,strsize] in the given base to limbs at rp }
function mpn_set_str(rp: mpn_ptr; str: pbyte; strsize: sizeuint; base:longint): mp_size_t; cdecl; external LIB name '__gmpn_set_str';
{ Compute the square root of [sp, n] and put the result at [r1p, dn/2e] and the remainder at [r2p, retval] }
function mpn_sqrtrem(r1p, r2p, sp: mpn_ptr; n: mp_size_t): mp_size_t; cdecl; external LIB name '__gmpn_sqrtrem';
{ Subtract [s2p, s2n] from [s1p, s1n], and write the s1n least significant limbs of the result to rp. Return borrow, either 0 or 1. }
function mpn_sub(rp, s1p: mpn_ptr; s1n: mp_size_t; s2p: mpn_ptr; s2n: mp_size_t): mp_limb_t; cdecl; external LIB name '__gmpn_sub';
{ Subtract s2limb from [s1p, n], and write the n least significant limbs of the result to rp. Return borrow, either 0 or 1. }
function mpn_sub_1(rp, s1p: mpn_ptr; n: mp_size_t; s2limb: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_sub_1';
{ Subtract [s2p, n] from [s1p, n], and write the n least significant limbs of the result to rp. Return borrow, either 0 or 1. }
function mpn_sub_n(rp, s1p, s2p: mpn_ptr; n: mp_size_t): mp_limb_t; cdecl; external LIB name '__gmpn_sub_n';
{ Multiply [s1p, n] and s2limb, and subtract the n least significant limbs of the product from [rp, n] and write the result to rp. Return the most significant limb of the product, plus borrow-out from the subtraction. }
function mpn_submul_1(rp, s1p: mpn_ptr; n: mp_size_t; s2limb: mp_limb_t): mp_limb_t; cdecl; external LIB name '__gmpn_submul_1';
{ Divide [np, nn] by [dp, dn] and put the quotient at [qp, nn−dn+1] and the remainder at [rp, dn] }
procedure mpn_tdiv_qr(qp, rp: mpn_ptr; qxn: mp_size_t; np: mpn_ptr; nn: mp_size_t; dp: mpn_ptr; dn: mp_size_t); cdecl; external LIB name '__gmpn_tdiv_qr';

// ---- GMP properties ----

{ Size of a limb on this machine }
function bits_per_limb: longint;
{ Some GMP functions may set this thread unsafe variable. Better avoid using it. }
function errno: longint;
{ GMP version string a.b.c }
function version: string;

// ==== ext bindings =====

// ---- Random Number Functions ----

{ Initialize state with a default algorithm }
procedure randinit_default(out state: MPRandState);
{ Initialize state with a linear congruential algorithm X = (aX + c) mod 2^m2exp }
procedure randinit_lc_2exp(out state: MPRandState; var a: MPInteger; c, m2exp: valuint);
{ Initialize state for a linear congruential algorithm as per gmp_randinit_lc_2exp }
function randinit_lc_2exp_size(out state: MPRandState; size: sizeuint): boolean;
{ Initialize state for a Mersenne Twister algorithm }
procedure randinit_mt(out state: MPRandState);
{ Initialize rop with a copy of the algorithm and state from op }
procedure randinit_set(out rop: MPRandState; var op: MPRandState);
{ Set an initial seed value into state }
procedure randseed(var state: MPRandState; var seed: MPInteger);
{ Set an initial seed value into state }
procedure randseed_ui(var state: MPRandState; seed: valuint);
{ Free all memory occupied by state }
procedure randclear(var state: MPRandState);
{ Return an uniformly distributed random number of n bits, ie. in the range 0 to 2^n−1 inclusive }
function urandomb_ui(var state: MPRandState; n: valuint): valuint;
{ Return an uniformly distributed random number in the range 0 to n − 1, inclusive }
function urandomm_ui(var state: MPRandState; n: valuint): valuint;

// ---- integer Functions ----

{ Change the space for integer to new_alloc limbs }
function z_realloc(var integer_: MPInteger; new_alloc: mp_size_t): pointer;
{ Set rop to the absolute value of op }
procedure z_abs(var rop, op: MPInteger);
{ Return the absolute value of op }
function z_abs(var op: MPInteger): MPInteger;
{ Set rop to op1 + op2 }
procedure z_add(var rop, op1, op2: MPInteger);
{ Return op1 + op2 }
function z_add(var op1, op2: MPInteger): MPInteger;
{ Set rop to op1 + op2 }
procedure z_add_ui(var rop, op1: MPInteger; op2: valuint);
{ Return op1 + op2 }
function z_add_ui(var op1: MPInteger; op2: valuint): MPInteger;
{ Set rop to rop + op1 × op2 }
procedure z_addmul(var rop, op1, op2: MPInteger);
{ Set rop to rop + op1 × op2 }
procedure z_addmul_ui(var rop, op1: MPInteger; op2: valuint);
{ Set rop to op1 bitwise-and op2 }
procedure z_and(var rop, op1, op2: MPInteger);
{ Return op1 bitwise-and op2 }
function z_and(var op1, op2: MPInteger): MPInteger;
//{ _Fixed_ space of fixed_num_bits is allocated to each of the array size integers in integer array }
//procedure z_array_init(var integer_array: MPInteger; array_size, fixed_num_bits: mp_size_t);
{ Compute the binomial coefficient (n over k) and store the result in rop }
procedure z_bin_ui(var rop, n: MPInteger; k: valuint);
{ Return the binomial coefficient (n over k) }
function z_bin_ui(var n: MPInteger; k: valuint): MPInteger;
{ Compute the binomial coefficient (n over k) and store the result in rop }
procedure z_bin_uiui(var rop: MPInteger; n, k: valuint);
{ Return the binomial coefficient (n over k) }
function z_bin_uiui(n, k: valuint): MPInteger;
{ Divide n by d, forming a quotient q. Round mode ceil. }
procedure z_cdiv_q(var q, n, d: MPInteger);
{ Divide n by d, forming a return quotient. Round mode ceil. }
function z_cdiv_q(var n, d: MPInteger): MPInteger;
{ Divide n by d, forming a quotient q. d = 2^b. Round mode ceil. }
procedure z_cdiv_q_2exp(var q, n: MPInteger; b: valuint);
{ Divide n by d, forming a return quotient. d = 2^b. Round mode ceil. }
function z_cdiv_q_2exp(var n: MPInteger; b: valuint): MPInteger;
{ Divide n by d, forming a quotient q. Round mode ceil. }
function z_cdiv_q_ui(var q, n: MPInteger; d: valuint): valuint;
{ Divide n by d, forming a quotient q and remainder r. Round mode ceil. }
procedure z_cdiv_qr(var q, r, n, d: MPInteger);
{ Divide n by d, forming a quotient q and remainder r. Round mode ceil. }
function z_cdiv_qr_ui(var q, r, n: MPInteger; d: valuint): valuint;
{ Divide n by d, forming a remainder r. Round mode ceil. }
procedure z_cdiv_r(var r, n, d: MPInteger);
{ Divide n by d, forming a return remainder. Round mode ceil. }
function z_cdiv_r(var n, d: MPInteger): MPInteger;
{ Divide n by d, forming a remainder r. d = 2^b. Round mode ceil. }
procedure z_cdiv_r_2exp(var r, n: MPInteger; b: valuint);
{ Divide n by d, forming a return remainder. d = 2^b. Round mode ceil. }
function z_cdiv_r_2exp(var n: MPInteger; b: valuint): MPInteger;
{ Divide n by d, forming a remainder r. Round mode ceil. }
function z_cdiv_r_ui(var r, n: MPInteger; d: valuint): valuint;
{ Divide n by d. Round mode ceil. }
function z_cdiv_ui(var n: MPInteger; d: valuint): valuint;
{ Free the space occupied by integer. Call this function for all MPInteger variables when you are done with them. }
procedure z_clear(var integer_: MPInteger);
{ Clear bit bit_index in rop }
procedure z_clrbit(var rop: MPInteger; bit_index: valuint);
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative value if op1 < op2. }
function z_cmp(var op1, op2: MPInteger): longint;
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative value if op1 < op2. }
function z_cmp_d(var op1: MPInteger; op2: double): longint;
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative value if op1 < op2. }
function z_cmp_si(var op1: MPInteger; op2: valsint): longint;
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative value if op1 < op2. }
function z_cmp_ui(var op1: MPInteger; op2: valuint): longint;
{ Compare the absolute values of op1 and op2. Return a positive value if |op1| > |op2|, zero if |op1| = |op2|, or a negative value if |op1| < |op2|. }
function z_cmpabs(var op1, op2: MPInteger): longint;
{ Compare the absolute values of op1 and op2. Return a positive value if |op1| > |op2|, zero if |op1| = |op2|, or a negative value if |op1| < |op2|. }
function z_cmpabs_d(var op1: MPInteger; op2: double): longint;
{ Compare the absolute values of op1 and op2. Return a positive value if |op1| > |op2|, zero if |op1| = |op2|, or a negative value if |op1| < |op2|. }
function z_cmpabs_ui(var op1: MPInteger; op2: valuint): longint;
{ Set rop to the one’s complement of op }
procedure z_com(var rop, op: MPInteger);
{ Return the one’s complement of op }
function z_com(var op: MPInteger): MPInteger;
{ Complement bit bit_index in rop }
procedure z_combit(var rop: MPInteger; bit_index: valuint);
{ Return true if n is congruent to c modulo d }
function z_congruent_p(var n, c, d: MPInteger): boolean;
{ Return true if n is congruent to c modulo 2^b }
function z_congruent_2exp_p(var n, c: MPInteger; b: valuint): boolean;
{ Return true if n is congruent to c modulo d }
function z_congruent_ui_p(var n: MPInteger; c, d: valuint): boolean;
{ Set q to n/d }
procedure z_divexact(var q, n, d: MPInteger);
{ Return n/d }
function z_divexact(var n, d: MPInteger): MPInteger;
{ Set q to n/d }
procedure z_divexact_ui(var q, n: MPInteger; d: valuint);
{ Return n/d }
function z_divexact_ui(var n: MPInteger; d: valuint): MPInteger;
{ Return true if n is exactly divisible by d }
function z_divisible_p(var n, d: MPInteger): boolean;
{ Return true if n is exactly divisible by d }
function z_divisible_ui_p(var n: MPInteger; d: valuint): boolean;
{ Return true if n is exactly divisible by by 2^b }
function z_divisible_2exp_p(var n: MPInteger; b: valuint): boolean;
{ Fill buf with word data from op }
function z_export(out buf; out countp: sizeuint; order: longint; size: sizeuint; endian: longint; nails: sizeuint; var op: MPInteger): pointer;
{ Set rop to op!, the factorial of op }
procedure z_fac_ui(var rop: MPInteger; op: valuint);
{ Return op!, the factorial of op }
function z_fac_ui(op: valuint): MPInteger;
{ Divide n by d, forming a quotient q. Round mode floor. }
procedure z_fdiv_q(var q, n, d: MPInteger);
{ Divide n by d, forming a return quotient. Round mode floor. }
function z_fdiv_q(var n, d: MPInteger): MPInteger;
{ Divide n by d, forming a quotient q. d = 2^b. Round mode floor. }
procedure z_fdiv_q_2exp(var q, n: MPInteger; b: valuint);
{ Divide n by d, forming a return quotient. d = 2^b. Round mode floor. }
function z_fdiv_q_2exp(var n: MPInteger; b: valuint): MPInteger;
{ Divide n by d, forming a quotient q. Round mode floor. }
function z_fdiv_q_ui(var q, n: MPInteger; d: valuint): valuint;
{ Divide n by d, forming a quotient q and remainder r. Round mode floor. }
procedure z_fdiv_qr(var q, r, n, d: MPInteger);
{ Divide n by d, forming a quotient q and remainder r. Round mode floor. }
function z_fdiv_qr_ui(var q, r, n: MPInteger; d: valuint): valuint;
{ Divide n by d, forming a remainder r. Round mode floor. }
procedure z_fdiv_r(var r, n, d: MPInteger);
{ Divide n by d, forming a return remainder. Round mode floor. }
function z_fdiv_r(var n, d: MPInteger): MPInteger;
{ Divide n by d, forming a remainder r. d = 2^b. Round mode floor. }
procedure z_fdiv_r_2exp(var r, n: MPInteger; b: valuint);
{ Divide n by d, forming a return remainder. d = 2^b. Round mode floor. }
function z_fdiv_r_2exp(var n: MPInteger; b: valuint): MPInteger;
{ Divide n by d, forming a remainder r. Round mode floor. }
function z_fdiv_r_ui(var r, n: MPInteger; d: valuint): valuint;
{ Divide n by d. Round mode floor. }
function z_fdiv_ui(var n: MPInteger; d: valuint): valuint;
{ Set fn to to Fn, the n’th Fibonacci number }
procedure z_fib_ui(var fn: MPInteger; n: valuint);
{ Return Fn, the n’th Fibonacci number }
function z_fib_ui(n: valuint): MPInteger;
{ Set fn to Fn, and fnsub1 to Fn−1 }
procedure z_fib2_ui(var fn, fnsub1: MPInteger; n: valuint);
{ Return Fn, and fnsub1 = Fn−1 }
function z_fib2_ui(var fnsub1: MPInteger; n: valuint): MPInteger;
{ Return true iff the value of op fits in an signed int }
function z_fits_sint_p(var op: MPInteger): boolean;
{ Return true iff the value of op fits in an signed long int }
function z_fits_slong_p(var op: MPInteger): boolean;
{ Return true iff the value of op fits in an signed short int }
function z_fits_sshort_p(var op: MPInteger): boolean;
{ Return true iff the value of op fits in an unsigned int }
function z_fits_uint_p(var op: MPInteger): boolean;
{ Return true iff the value of op fits in an unsigned long int }
function z_fits_ulong_p(var op: MPInteger): boolean;
{ Return true iff the value of op fits in an unsigned short int }
function z_fits_ushort_p(var op: MPInteger): boolean;
{ Set rop to the greatest common divisor of op1 and op2 }
procedure z_gcd(var rop, op1, op2: MPInteger);
{ Return the greatest common divisor of op1 and op2 }
function z_gcd(var op1, op2: MPInteger): MPInteger;
{ Compute the greatest common divisor of op1 and op2 }
function z_gcd_ui(var rop, op1: MPInteger; op2: valuint): valuint;
{ Set g to the greatest common divisor of a and b, and in addition set s and t to coefficients satisfying as + bt = g }
procedure z_gcdext(var g, s, t, a, b: MPInteger);
{ Convert op to a double, truncating if necessary (ie. rounding towards zero) }
function z_get_d(var op: MPInteger): double;
{ Convert op to a double, truncating if necessary (ie. rounding towards zero), and returning the exponent separately }
function z_get_d_2exp(out exp: valsint; var op: MPInteger): double;
{ Return the value of op as a signed long }
function z_get_si(op: MPInteger): valsint;
{ Convert op to a string of digits in base base. The base argument may vary from 2 to 62 or from −2 to −36. }
function z_get_str(base: longint; var op: MPInteger): string;
{ Convert op to a string of digits in base base. The base argument may vary from 2 to 62 or from −2 to −36. If str is NULL, the result string is allocated using the current allocation function }
function z_get_str(str: pchar; base: longint; var op: MPInteger): pchar;
{ Return the value of op as an unsigned long }
function z_get_ui(op: MPInteger): valuint;
{ Return limb number n from op }
function z_getlimbn(var op: MPInteger; n: mp_size_t): mp_limb_t;
{ If op1 and op2 are both >= 0 or both < 0, return the hamming distance between the two operands, which is the number of bit positions where op1 and op2 have different bit values }
function z_hamdist(var op1, op2: MPInteger): valuint;
{ Set rop from an array of word data at op }
procedure z_import(var rop: MPInteger; count: sizeuint; order: longint; size: sizeuint; endian: longint; nails: sizeuint; const op);
{ Initialize integer, and set its value to 0 }
procedure z_init(out integer_: MPInteger);
{ Initialize integer, with space for n bits, and set its value to 0 }
procedure z_init2(out integer_: MPInteger; n: valuint);
{ Initialize rop with limb space and set the initial numeric value from op }
procedure z_init_set(out rop: MPInteger; var op: MPInteger);
{ Initialize rop with limb space and set the initial numeric value from op }
procedure z_init_set_d(out rop: MPInteger; op: double);
{ Initialize rop with limb space and set the initial numeric value from op }
procedure z_init_set_si(out rop: MPInteger; op: valsint);
{ Initialize rop and set its value like z_set_str. If the string is a correct base base number, the function returns true. }
function z_init_set_str(out rop: MPInteger; str: string; base: longint): boolean;
{ Initialize rop with limb space and set the initial numeric value from op }
procedure z_init_set_ui(out rop: MPInteger; op: valuint);
{ Compute the inverse of op1 modulo op2 and put the result in rop }
function z_invert(var rop, op1, op2: MPInteger): longint;
{ Set rop to op1 bitwise inclusive-or op2 }
procedure z_ior(var rop, op1, op2: MPInteger);
{ Return bitwise inclusive-or op2 }
function z_ior(var op1, op2: MPInteger): MPInteger;
{ Calculate the Jacobi symbol. This is defined only for b odd }
function z_jacobi(var a, b: MPInteger): longint;
{ Calculate the Jacobi symbol with the Kronecker extension }
function z_kronecker_si(var a: MPInteger; b: valsint): longint;
{ Calculate the Jacobi symbol with the Kronecker extension }
function z_kronecker_ui(var a: MPInteger; b: valuint): longint;
{ Calculate the Jacobi symbol with the Kronecker extension }
function z_si_kronecker(a: valsint; var b: MPInteger): longint;
{ Calculate the Jacobi symbol with the Kronecker extension }
function z_ui_kronecker(a: valuint; var b: MPInteger): longint;
{ Set rop to the least common multiple of op1 and op2 }
procedure z_lcm(var rop, op1, op2: MPInteger);
{ Return the least common multiple of op1 and op2 }
function z_lcm(var op1, op2: MPInteger): MPInteger;
{ Set rop to the least common multiple of op1 and op2 }
procedure z_lcm_ui(var rop, op1: MPInteger; op2: valuint);
{ Return the least common multiple of op1 and op2 }
function z_lcm_ui(var op1: MPInteger; op2: valuint): MPInteger;
{ Set ln to to Ln, the n’th Lucas number }
procedure z_lucnum_ui(var ln: MPInteger; n: valuint);
{ Return Ln, the n’th Lucas number }
function z_lucnum_ui(n: valuint): MPInteger;
{ Set ln to Ln, and lnsub1 to Ln−1 }
procedure z_lucnum2_ui(var ln, lnsub1: MPInteger; n: valuint);
{ Return Ln, and lnsub1 to Ln−1 }
function z_lucnum2_ui(var lnsub1: MPInteger; n: valuint): MPInteger;
{ Set r to n mod d. The sign of the divisor is ignored; the result is always non-negative. }
procedure z_mod(var r, n, d: MPInteger);
{ Return n mod d. The sign of the divisor is ignored; the result is always non-negative. }
function z_mod(var n, d: MPInteger): MPInteger;
{ Set rop to op1 × op2 }
procedure z_mul(var rop, op1, op2: MPInteger);
{ Return op1 × op2 }
function z_mul(var op1, op2: MPInteger): MPInteger;
{ Set rop to op1 × 2^op2. This operation can also be defined as a left shift by op2 bits. }
procedure z_mul_2exp(var rop, op1: MPInteger; op2: valuint);
{ Return op1 × 2^op2. This operation can also be defined as a left shift by op2 bits. }
function z_mul_2exp(var op1: MPInteger; op2: valuint): MPInteger;
{ Set rop to op1 × op2 }
procedure z_mul_si(var rop, op1: MPInteger; op2: valsint);
{ Return op1 × op2 }
function z_mul_si(var op1: MPInteger; op2: valsint): MPInteger;
{ Set rop to op1 × op2 }
procedure z_mul_ui(var rop, op1: MPInteger; op2: valuint);
{ Return op1 × op2 }
function z_mul_ui(var op1: MPInteger; op2: valuint): MPInteger;
{ Set rop to −op }
procedure z_neg(var rop, op: MPInteger);
{ Return −op }
function z_neg(var op: MPInteger): MPInteger;
{ Set rop to the next prime greater than op }
procedure z_nextprime(var rop, op: MPInteger);
{ Return the next prime greater than op }
function z_nextprime(var op: MPInteger): MPInteger;
{ Return true if op is a perfect power, i.e., if there exist integers a and b, with b > 1, such that op = a^b }
function z_perfect_power_p(var op: MPInteger): boolean;
{ Return true if op is a perfect square, i.e., if the square root of op is an integer }
function z_perfect_square_p(var op: MPInteger): boolean;
{ If op >= 0, return the population count of op, which is the number of 1 bits in the binary representation }
function z_popcount(var op: MPInteger): valuint;
{ Set rop to base^exp. The case 0^0 yields 1. }
procedure z_pow_ui(var rop, base: MPInteger; exp: valuint);
{ Return base^exp. The case 0^0 yields 1. }
function z_pow_ui(var base: MPInteger; exp: valuint): MPInteger;
{ Set rop to base^exp mod mod_ }
procedure z_powm(var rop, base, exp, mod_: MPInteger);
{ Return base^exp mod mod_ }
function z_powm(var base, exp, mod_: MPInteger): MPInteger;
{ Set rop to base^exp mod mod_ }
procedure z_powm_ui(var rop, base: MPInteger; exp: valuint; var mod_: MPInteger);
{ Return base^exp mod mod_ }
function z_powm_ui(var base: MPInteger; exp: valuint; var mod_: MPInteger): MPInteger;
{ Determine whether n is prime. Return 2 if n is definitely prime, return 1 if n is probably prime (without being certain), or return 0 if n is definitely composite. }
function z_probab_prime_p(var n: MPInteger; reps: longint): longint;
{ Change the space allocated for integer to n bits. The value in integer is preserved if it fits, or is set to 0 if not. }
procedure z_realloc2(var integer_: MPInteger; n: valuint);
{ Remove all occurrences of the factor f from op and store the result in rop }
function z_remove(var rop, op, f: MPInteger): valuint;
{ Set rop to trunc(op^(1/n)), the truncated integer part of the nth root of op. Return true if the computation was exact, i.e., if op is rop to the nth power. }
function z_root(var rop, op: MPInteger; n: valuint): boolean;
{ Set root to trunc(u^(1/n)), the truncated integer part of the nth root of u. Set rem to the remainder, (u − root^n). }
procedure z_rootrem(var root, rem, u: MPInteger; n: valuint);
{ Generate a random integer with long strings of zeros and ones in the binary representation }
procedure z_rrandomb(var rop: MPInteger; var state: MPRandState; n: valuint);
{ Return a random integer with long strings of zeros and ones in the binary representation }
function z_rrandomb(var state: MPRandState; n: valuint): MPInteger;
{ Scan op, starting from bit starting_bit, towards more significant bits, until the first 0 bit is found }
function z_scan0(var op: MPInteger; starting_bit: valuint): valuint;
{ Scan op, starting from bit starting_bit, towards more significant bits, until the first 1 bit is found }
function z_scan1(var op: MPInteger; starting_bit: valuint): valuint;
{ Set the value of rop from op }
procedure z_set(var rop, op: MPInteger);
{ Set the value of rop from op }
procedure z_set_d(var rop: MPInteger; op: double);
{ Set the value of rop from op }
procedure z_set_f(var rop: MPInteger; var op: MPFloat);
{ Set the value of rop from op }
procedure z_set_q(var rop: MPInteger; var op: MPRational);
{ Set the value of rop from op }
procedure z_set_si(var rop: MPInteger; op: valsint);
{ Set the value of rop from str, a null-terminated C string in base base. If the string is a correct base base number, the function returns true. }
function z_set_str(var rop: MPInteger; str: string; base: longint): boolean;
{ Set the value of rop from op }
procedure z_set_ui(var rop: MPInteger; op: valuint);
{ Set bit bit_index in rop }
procedure z_setbit(var rop: MPInteger; bit_index: valuint);
{ Return the size of op measured in number of limbs }
function z_size(var op: MPInteger): sizeuint;
{ Return the size of op measured in number of digits in the given base }
function z_sizeinbase(var op: MPInteger; base: longint): sizeuint;
{ Set rop to trunc(sqrt(op)), the truncated integer part of the square root of op }
procedure z_sqrt(var rop, op: MPInteger);
{ Return trunc(sqrt(op)), the truncated integer part of the square root of op }
function z_sqrt(var op: MPInteger): MPInteger;
{ Set rop1 to trunc(sqrt(op)), like z_sqrt. Set rop2 to the remainder (op − rop1^2), which will be zero if op is a perfect square. }
procedure z_sqrtrem(var rop1, rop2, op: MPInteger);
{ Set rop to op1 − op2 }
procedure z_sub(var rop, op1, op2: MPInteger);
{ Return op1 − op2 }
function z_sub(var op1, op2: MPInteger): MPInteger;
{ Set rop to op1 − op2 }
procedure z_sub_ui(var rop, op1: MPInteger; op2: valuint);
{ Return op1 − op2 }
function z_sub_ui(var op1: MPInteger; op2: valuint): MPInteger;
{ Set rop to op1 − op2 }
procedure z_ui_sub(var rop: MPInteger; op1: valuint; var op2: MPInteger);
{ Return op1 − op2 }
function z_ui_sub(op1: valuint; var op2: MPInteger): MPInteger;
{ Set rop to rop − op1 × op2 }
procedure z_submul(var rop, op1, op2: MPInteger);
{ Set rop to rop − op1 × op2 }
procedure z_submul_ui(var rop, op1: MPInteger; op2: valuint);
{ Swap the values rop1 and rop2 efficiently }
procedure z_swap(var rop1, rop2: MPInteger);
{ Divide n by d, forming a quotient q. Round mode trunc. }
procedure z_tdiv_q(var q, n, d: MPInteger);
{ Divide n by d, forming a return quotient. Round mode trunc. }
function z_tdiv_q(var n, d: MPInteger): MPInteger;
{ Divide n by d, forming a quotient q. d = 2^b. Round mode trunc. }
procedure z_tdiv_q_2exp(var q, n: MPInteger; b: valuint);
{ Divide n by d, forming a return quotient. d = 2^b. Round mode trunc. }
function z_tdiv_q_2exp(var n: MPInteger; b: valuint): MPInteger;
{ Divide n by d, forming a quotient q. Round mode trunc. }
function z_tdiv_q_ui(var q, n: MPInteger; d: valuint): valuint;
{ Divide n by d, forming a quotient q and remainder r. Round mode trunc. }
procedure z_tdiv_qr(var q, r, n, d: MPInteger);
{ Divide n by d, forming a quotient q and remainder r. Round mode trunc. }
function z_tdiv_qr_ui(var q, r, n: MPInteger; d: valuint): valuint;
{ Divide n by d, forming a remainder r. Round mode trunc. }
procedure z_tdiv_r(var r, n, d: MPInteger);
{ Divide n by d, forming a return remainder. Round mode trunc. }
function z_tdiv_r(var n, d: MPInteger): MPInteger;
{ Divide n by d, forming a remainder r. d = 2^b. Round mode trunc. }
procedure z_tdiv_r_2exp(var r, n: MPInteger; b: valuint);
{ Divide n by d, forming a return remainder. d = 2^b. Round mode trunc. }
function z_tdiv_r_2exp(var n: MPInteger; b: valuint): MPInteger;
{ Divide n by d, forming a remainder r. Round mode trunc. }
function z_tdiv_r_ui(var r, n: MPInteger; d: valuint): valuint;
{ Divide n by d. Round mode trunc. }
function z_tdiv_ui(var n: MPInteger; d: valuint): valuint;
{ Test bit bit_index in op and return true or false accordingly }
function z_tstbit(var rop: MPInteger; bit_index: valuint): boolean;
{ Set rop to base^exp. The case 0^0 yields 1 }
procedure z_ui_pow_ui(var rop: MPInteger; base, exp: valuint);
{ Return base^exp. The case 0^0 yields 1 }
function z_ui_pow_ui(base, exp: valuint): MPInteger;
{ Generate a uniformly distributed random integer in the range 0 to 2^n − 1, inclusive }
procedure z_urandomb(var rop: MPInteger; var state: MPRandState; n: valuint);
{ Return a uniformly distributed random integer in the range 0 to 2^n − 1, inclusive }
function z_urandomb(var state: MPRandState; n: valuint): MPInteger;
{ Generate a uniform random integer in the range 0 to n − 1, inclusive }
procedure z_urandomm(var rop: MPInteger; var state: MPRandState; var n: MPInteger);
{ Return a uniform random integer in the range 0 to n − 1, inclusive }
function z_urandomm(var state: MPRandState; var n: MPInteger): MPInteger;
{ Set rop to op1 bitwise exclusive-or op2 }
procedure z_xor(var rop, op1, op2: MPInteger);
{ Retuen op1 bitwise exclusive-or op2 }
function z_xor(var op1, op2: MPInteger): MPInteger;

// ---- Rational Number Functions ----

{ Set rop to the absolute value of op }
procedure q_abs(var rop, op: MPRational);
{ Return absolute value of op }
function q_abs(var op: MPRational): MPRational;
{ Set sum to addend1 + addend2 }
procedure q_add(var sum, addend1, addend2: MPRational);
{ Return addend1 + addend2 }
function q_add(var addend1, addend2: MPRational): MPRational;
{ Remove any factors that are common to the numerator and denominator of op, and make the denominator positive }
procedure q_canonicalize(var op: MPRational);
{ Free the space occupied by rational number }
procedure q_clear(var rational_number: MPRational);
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2 }
function q_cmp(var op1, op2: MPRational): longint;
{ Compare op1 and num2/den2. Return a positive value if op1 > num2/den2, zero if op1 = num2/den2, and a negative value if op1 < num2/den2 }
function q_cmp_si(var op1: MPRational; num2: valsint; den2: valuint): longint;
{ Compare op1 and num2/den2. Return a positive value if op1 > num2/den2, zero if op1 = num2/den2, and a negative value if op1 < num2/den2 }
function q_cmp_ui(var op1: MPRational; num2, den2: valuint): longint;
{ Set quotient to dividend/divisor }
procedure q_div(var quotient, dividend, divisor: MPRational);
{ Return dividend/divisor }
function q_div(var dividend, divisor: MPRational): MPRational;
{ Set rop to op1/(2^op2) }
procedure q_div_2exp(var rop, op1: MPRational; op2: valuint);
{ Return op1/(2^op2) }
function q_div_2exp(var op1: MPRational; op2: valuint): MPRational;
{ Return true if op1 and op2 are equal, false if they are non-equal }
function q_equal(var op1, op2: MPRational): boolean;
{ Get the numerator of a rational }
procedure q_get_num(var numerator: MPInteger; var rational: MPRational);
{ Return the numerator of a rational }
function q_get_num(var rational: MPRational): MPInteger;
{ Get the denominator of a rational }
procedure q_get_den(var denominator: MPInteger; var rational: MPRational);
{ Return the denominator of a rational }
function q_get_den(var rational: MPRational): MPInteger;
{ Convert op to a double, truncating if necessary (ie. rounding towards zero) }
function q_get_d(var op: MPRational): double;
{ Convert op to a string of digits in base base }
function q_get_str(base: longint; var op: MPRational): string;
{ Convert op to a string of digits in base base. If str is NULL, the result string is allocated using the current allocation function. }
function q_get_str(str: pchar; base: longint; var op: MPRational): pchar;
{ Initialize dest rational and set it to 0/1 }
procedure q_init(out dest_rational: MPRational);
{ Set inverted_number to 1/number }
procedure q_inv(var inverted_number, number: MPRational);
{ Return 1/number }
function q_inv(var number: MPRational): MPRational;
{ Set product to multiplier × multiplicand }
procedure q_mul(var product, multiplier, multiplicand: MPRational);
{ Return multiplier × multiplicand }
function q_mul(var multiplier, multiplicand: MPRational): MPRational;
{ Set rop to op1 × (2^op2) }
procedure q_mul_2exp(var rop, op1: MPRational; op2: valuint);
{ Return op1 × (2^op2) }
function q_mul_2exp(var op1: MPRational; op2: valuint): MPRational;
{ Set negated_operand to −operand }
procedure q_neg(var negated_operand, operand: MPRational);
{ Return −operand }
function q_neg(var operand: MPRational): MPRational;
{ Assign rop from op }
procedure q_set(var rop, op: MPRational);
{ Set rop to the value of op. There is no rounding, this conversion is exact. }
procedure q_set_d(var rop: MPRational; op: double);
{ Set the denominator of a rational }
procedure q_set_den(var rational: MPRational; var denominator: MPInteger);
{ Set rop to the value of op. There is no rounding, this conversion is exact. }
procedure q_set_f(var rop: MPRational; var op: MPFloat);
{ Set the numerator of a rational }
procedure q_set_num(var rational: MPRational; var numerator: MPInteger);
{ Set the value of rop to op1/op2 }
procedure q_set_si(var rop: MPRational; op1: valsint; op2: valuint);
{ Set rop from a null-terminated string str in the given base. The return value is true if the entire string is a valid number. }
function q_set_str(var rop: MPRational; str: string; base: longint): boolean;
{ Set the value of rop to op1/op2 }
procedure q_set_ui(var rop: MPRational; op1, op2: valuint);
{ Assign rop from op }
procedure q_set_z(var rop: MPRational; var op: MPInteger);
{ Set difference to minuend − subtrahend }
procedure q_sub(var difference, minuend, subtrahend: MPRational);
{ Return minuend − subtrahend }
function q_sub(var minuend, subtrahend: MPRational): MPRational;
{ Swap the values rop1 and rop2 efficiently }
procedure q_swap(var rop1, rop2: MPRational);

// ---- Floating-point Functions ----

{ Set rop to the absolute value of op }
procedure f_abs(var rop, op: MPFloat);
{ Return the absolute value of op }
function f_abs(var op: MPFloat): MPFloat;
{ Set rop to op1 + op2 }
procedure f_add(var rop, op1, op2: MPFloat);
{ Return op1 + op2 }
function f_add(var op1, op2: MPFloat): MPFloat;
{ Set rop to op1 + op2 }
procedure f_add_ui(var rop, op1: MPFloat; op2: valuint);
{ Return op1 + op2 }
function f_add_ui(var op1: MPFloat; op2: valuint): MPFloat;
{ Set rop to op rounded to the next higher integer }
procedure f_ceil(var rop, op: MPFloat);
{ Return op rounded to the next higher integer }
function f_ceil(var op: MPFloat): MPFloat;
{ Free the space occupied by x }
procedure f_clear(var x: MPFloat);
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. }
function f_cmp(var op1, op2: MPFloat): longint;
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. }
function f_cmp_d(var op1: MPFloat; op2: double): longint;
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. }
function f_cmp_si(var op1: MPFloat; op2: valsint): longint;
{ Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. }
function f_cmp_ui(var op1: MPFloat; op2: valuint): longint;
{ Set rop to op1/op2 }
procedure f_div(var rop, op1, op2: MPFloat);
{ Return op1/op2 }
function f_div(var op1, op2: MPFloat): MPFloat;
{ Set rop to op1/(2^op2) }
procedure f_div_2exp(var rop, op1: MPFloat; op2: valuint);
{ Return op1/(2^op2) }
function f_div_2exp(var op1: MPFloat; op2: valuint): MPFloat;
{ Set rop to op1/op2 }
procedure f_div_ui(var rop, op1: MPFloat; op2: valuint);
{ Return op1/op2 }
function f_div_ui(var op1: MPFloat; op2: valuint): MPFloat;
{ Return true if the first op3 bits of op1 and op2 are equal, false otherwise }
function f_eq(var op1, op2: MPFloat; op3: valuint): boolean;
{ Return true if op would fit in the respective C data type, when truncated to an integer }
function f_fits_sint_p(var op: MPFloat): boolean;
{ Return true if op would fit in the respective C data type, when truncated to an integer }
function f_fits_slong_p(var op: MPFloat): boolean;
{ Return true if op would fit in the respective C data type, when truncated to an integer }
function f_fits_sshort_p(var op: MPFloat): boolean;
{ Return true if op would fit in the respective C data type, when truncated to an integer }
function f_fits_uint_p(var op: MPFloat): boolean;
{ Return true if op would fit in the respective C data type, when truncated to an integer }
function f_fits_ulong_p(var op: MPFloat): boolean;
{ Return true if op would fit in the respective C data type, when truncated to an integer }
function f_fits_ushort_p(var op: MPFloat): boolean;
{ Set rop to op rounded to the next lower }
procedure f_floor(var rop, op: MPFloat);
{ Return op rounded to the next lower }
function f_floor(var op: MPFloat): MPFloat;
{ Convert op to a double, truncating if necessary (ie. rounding towards zero) }
function f_get_d(var op: MPFloat): double;
{ Convert op to a double, truncating if necessary (ie. rounding towards zero), and with an exponent returned separately }
function f_get_d_2exp(out exp: valsint; var op: MPFloat): double;
{ Return the default precision actually used }
function f_get_default_prec: valuint;
{ Return the current precision of op, in bits }
function f_get_prec(var op: MPFloat): valuint;
{ Convert op to a long, truncating any fraction part }
function f_get_si(var op: MPFloat): valsint;
{ Convert op to a string of digits in base base }
function f_get_str(out exp: mp_exp_t; base: longint; ndigits: sizeuint; var op: MPFloat): string;
{ Convert op to a string of digits in base base. If str is NULL, the result string is allocated using the current allocation function. }
function f_get_str(str: pchar; out exp: mp_exp_t; base: longint; ndigits: sizeuint; var op: MPFloat): pchar;
{ Convert op to a unsigned long, truncating any fraction part }
function f_get_ui(var op: MPFloat): valuint;
{ Initialize x to 0 }
procedure f_init(out x: MPFloat);
{ Initialize x to 0 and set its precision to be at least prec bits }
procedure f_init2(out x: MPFloat; prec: valuint);
{ Initialize rop and set its value from op }
procedure f_init_set(out rop: MPFloat; var op: MPFloat);
{ Initialize rop and set its value from op }
procedure f_init_set_d(out rop: MPFloat; op: double);
{ Initialize rop and set its value from op }
procedure f_init_set_si(out rop: MPFloat; op: valsint);
{ Initialize rop and set its value from the string in str. Returns true if the entire string is a valid number in base base. }
function f_init_set_str(out rop: MPFloat; str: string; base: longint): boolean;
{ Initialize rop and set its value from op }
procedure f_init_set_ui(out rop: MPFloat; op: valuint);
{ Return true if op is an integer }
function f_integer_p(var op: MPFloat): boolean;
{ Set rop to op1 × op2 }
procedure f_mul(var rop, op1, op2: MPFloat);
{ Return op1 × op2 }
function f_mul(var op1, op2: MPFloat): MPFloat;
{ Set rop to op1 × (2^op2) }
procedure f_mul_2exp(var rop, op1: MPFloat; op2: valuint);
{ Return op1 × (2^op2) }
function f_mul_2exp(var op1: MPFloat; op2: valuint): MPFloat;
{ Set rop to op1 × op2 }
procedure f_mul_ui(var rop, op1: MPFloat; op2: valuint);
{ Return op1 × op2 }
function f_mul_ui(var op1: MPFloat; op2: valuint): MPFloat;
{ Set rop to −op }
procedure f_neg(var rop, op: MPFloat);
{ Return −op }
function f_neg(var op: MPFloat): MPFloat;
{ Set rop to op1^op2 }
procedure f_pow_ui(var rop, op1: MPFloat; op2: valuint);
{ Return op1^op2 }
function f_pow_ui(var op1: MPFloat; op2: valuint): MPFloat;
{ Generate a random float of at most max_size limbs, with long strings of zeros and ones in the binary representation }
procedure f_random2(var rop: MPFloat; max_size: mp_size_t; exp: mp_exp_t);
{ Return a random float of at most max_size limbs, with long strings of zeros and ones in the binary representation }
function f_random2(max_size: mp_size_t; exp: mp_exp_t): MPFloat;
{ Compute the relative difference between op1 and op2 and store the result in rop }
procedure f_reldiff(var rop, op1, op2: MPFloat);
{ Return the relative difference between op1 and op2 }
function f_reldiff(var op1, op2: MPFloat): MPFloat;
{ Set the value of rop from op }
procedure f_set(var rop, op: MPFloat);
{ Set the value of rop from op }
procedure f_set_d(var rop: MPFloat; op: double);
{ Set the default precision to be at least prec bits }
procedure f_set_default_prec(prec: valuint);
{ Set the precision of rop to be at least prec bits }
procedure f_set_prec(var rop: MPFloat; prec: valuint);
{ Set the precision of rop to be at least prec bits, without changing the memory allocated }
procedure f_set_prec_raw(var rop: MPFloat; prec: valuint);
{ Set the value of rop from op }
procedure f_set_q(var rop: MPFloat; var op: MPRational);
{ Set the value of rop from op }
procedure f_set_si(var rop: MPFloat; op: valsint);
{ Set the value of rop from the string in str. Returns true if the entire string is a valid number in base base. }
function f_set_str(var rop: MPFloat; str: string; base: longint): boolean;
{ Set the value of rop from op }
procedure f_set_ui(var rop: MPFloat; op: valuint);
{ Set the value of rop from op }
procedure f_set_z(var rop: MPFloat; var op: MPInteger);
{ Set rop to op^(1/2) }
procedure f_sqrt(var rop, op: MPFloat);
{ Return op^(1/2) }
function f_sqrt(var op: MPFloat): MPFloat;
{ Set rop to op^(1/2) }
procedure f_sqrt_ui(var rop: MPFloat; op: valuint);
{ Return op^(1/2) }
function f_sqrt_ui(op: valuint): MPFloat;
{ Set rop to op1 − op2 }
procedure f_sub(var rop, op1, op2: MPFloat);
{ Return op1 − op2 }
function f_sub(var op1, op2: MPFloat): MPFloat;
{ Set rop to op1 − op2 }
procedure f_sub_ui(var rop, op1: MPFloat; op2: valuint);
{ Return op1 − op2 }
function f_sub_ui(var op1: MPFloat; op2: valuint): MPFloat;
{ Swap rop1 and rop2 efficiently }
procedure f_swap(var rop1, rop2: MPFloat);
{ Set rop to op rounded to the integer towards zero }
procedure f_trunc(var rop, op: MPFloat);
{ Return op rounded to the integer towards zero }
function f_trunc(var op: MPFloat): MPFloat;
{ Set rop to op1/op2 }
procedure f_ui_div(var rop: MPFloat; op1: valuint; var op2: MPFloat);
{ Return op1/op2 }
function f_ui_div(op1: valuint; var op2: MPFloat): MPFloat;
{ Set rop to op1 − op2 }
procedure f_ui_sub(var rop: MPFloat; op1: valuint; var op2: MPFloat);
{ Return op1 − op2 }
function f_ui_sub(op1: valuint; var op2: MPFloat): MPFloat;
{ Generate a uniformly distributed random float in rop, such that 0 <= rop < 1, with nbits significant bits in the mantissa }
procedure f_urandomb(var rop: MPFloat; var state: MPRandState; nbits: valuint);
{ Return a uniformly distributed random float, such that 0 <= result < 1, with nbits significant bits in the mantissa }
function f_urandomb(var state: MPRandState; nbits: valuint): MPFloat;

// ---- operators ----

operator * (op1: MPFloat; op2: MPFloat): MPFloat; inline;
operator * (op1: MPInteger; op2: MPInteger): MPInteger; inline;
operator * (op1: MPRational; op2: MPRational): MPRational; inline;
operator ** (op1: MPFloat; op2: valuint): MPFloat; inline;
operator ** (op1: MPInteger; op2: valuint): MPInteger; inline;
operator + (op1: MPFloat; op2: MPFloat): MPFloat; inline;
operator + (op1: MPInteger; op2: MPInteger): MPInteger; inline;
operator + (op1: MPRational; op2: MPRational): MPRational; inline;
operator - (op: MPFloat): MPFloat; inline;
operator - (op: MPInteger): MPInteger; inline;
operator - (op: MPRational): MPRational; inline;
operator - (op1: MPFloat; op2: MPFloat): MPFloat; inline;
operator - (op1: MPInteger; op2: MPInteger): MPInteger; inline;
operator - (op1: MPRational; op2: MPRational): MPRational; inline;
operator / (op1: MPFloat; op2: MPFloat): MPFloat; inline;
operator / (op1: MPInteger; op2: MPInteger): MPInteger; inline;
operator / (op1: MPRational; op2: MPRational): MPRational; inline;
operator := (op: double): MPFloat; inline;
operator := (op: double): MPInteger; inline;
operator := (op: double): MPRational; inline;
operator := (op: MPFloat): Cardinal; inline;
operator := (op: MPFloat): double; inline;
operator := (op: MPFloat): integer; inline;
operator := (op: MPFloat): mpf_t; inline;
operator := (op: MPFloat): MPInteger; inline;
operator := (op: MPFloat): MPRational; inline;
operator := (op: MPFloat): string; inline;
{$ifdef CPU64}
operator := (op: MPFloat): valsint; inline;
operator := (op: MPFloat): valuint; inline;
{$endif}
operator := (var op: mpf_t): MPFloat; inline;
operator := (op: MPInteger): cardinal; inline;
operator := (op: MPInteger): double; inline;
operator := (op: MPInteger): integer; inline;
operator := (op: MPInteger): MPFloat; inline;
operator := (op: MPInteger): MPRational; inline;
operator := (op: MPInteger): mpz_t; inline;
operator := (op: MPInteger): string; inline;
{$ifdef CPU64}
operator := (op: MPInteger): valsint; inline;
operator := (op: MPInteger): valuint; inline;
{$endif}
operator := (var op: mpq_t): MPRational; inline;
operator := (op: MPRandState): randstate_t; inline;
operator := (op: MPRational): double; inline;
operator := (op: MPRational): MPFloat; inline;
operator := (op: MPRational): MPInteger; inline;
operator := (op: MPRational): mpq_t; inline;
operator := (op: MPRational): string; inline;
operator := (var op: mpz_t): MPInteger; inline;
operator := (var op: randstate_t): MPRandState; inline;
operator := (op: string): MPFloat; inline;
operator := (op: string): MPInteger; inline;
operator := (op: string): MPRational; inline;
operator := (op: valsint): MPFloat; inline;
operator := (op: valsint): MPInteger; inline;
operator := (op: valsint): MPRational; inline;
operator := (op: valuint): MPFloat; inline;
operator := (op: valuint): MPInteger; inline;
operator := (op: valuint): MPRational; inline;
operator < (op1: MPFloat; op2: MPFloat): boolean; inline;
operator < (op1: MPInteger; op2: MPInteger): boolean; inline;
operator < (op1: MPRational; op2: MPRational): boolean; inline;
operator <= (op1: MPFloat; op2: MPFloat): boolean; inline;
operator <= (op1: MPInteger; op2: MPInteger): boolean; inline;
operator <= (op1: MPRational; op2: MPRational): boolean; inline;
operator > (op1: MPFloat; op2: MPFloat): boolean; inline;
operator > (op1: MPInteger; op2: MPInteger): boolean; inline;
operator > (op1: MPRational; op2: MPRational): boolean; inline;
operator >= (op1: MPFloat; op2: MPFloat): boolean; inline;
operator >= (op1: MPInteger; op2: MPInteger): boolean; inline;
operator >= (op1: MPRational; op2: MPRational): boolean; inline;
// compiler doesn't like theese
// operator = (op1: MPFloat; op2: MPFloat): boolean; inline;
// operator = (op1: MPInteger; op2: MPInteger): boolean; inline;
// operator = (op1: MPRational; op2: MPRational): boolean; inline;

implementation

uses
  math;

{$ifndef NO_GMP_GLOBVARS}
var
  __gmp_bits_per_limb: longint; cvar; external;
  __gmp_errno: longint; cvar; external;
  __gmp_version: pchar; cvar; external;

function bits_per_limb: longint;
begin
  result := __gmp_bits_per_limb;
end;

function errno: longint;
begin
  result := __gmp_errno;
end;

function version: string;
begin
  result := __gmp_version;
end;

{$else NO_GMP_GLOBVARS}
function bits_per_limb: longint;
const BITS_PER_BYTE = 8;
begin
  result := sizeof(mp_limb_t) * BITS_PER_BYTE;
end;

function errno: longint;
begin
  result := 0;
end;

function version: string;
const NO_VER = '0.0.0';
begin
  result := NO_VER;
end;
{$endif NO_GMP_GLOBVARS}

// ---- ext types ----

{ TMPBase }

function TMPBase.refs: longint;
begin
  result := frefcount;
end;

{ TMPInteger }

function TMPInteger.ptr: mpz_ptr;
begin
  result := @fmpz
end;

destructor TMPInteger.destroy;
begin
  mpz_clear(fmpz);
  inherited destroy;
end;

{ TMPFloat }

function TMPFloat.ptr: mpf_ptr;
begin
  result := @fmpf;
end;

destructor TMPFloat.destroy;
begin
  mpf_clear(fmpf);
  inherited destroy;
end;

{ TMPRational }

function TMPRational.ptr: mpq_ptr;
begin
  result := @fmpq;
end;

destructor TMPRational.destroy;
begin
  mpq_clear(fmpq);
  inherited destroy;
end;

{ TMPRandState }

function TMPRandState.ptr: randstate_ptr;
begin
  result := @frandstate;
end;

destructor TMPRandState.destroy;
begin
  mp_randclear(frandstate);
  inherited destroy;
end;

// --- helpers ----

function dest(var rop: MPInteger): mpz_ptr;
begin
  if (not assigned(rop)) or (rop.refs > 1) then
    z_init(rop);
  result := rop.ptr;
end;

function dest(var rop: MPFloat): mpf_ptr;
begin
  if (not assigned(rop)) or (rop.refs > 1) then
    f_init(rop);
  result := rop.ptr;
end;

function dest(var rop: MPRational): mpq_ptr;
begin
  if (not assigned(rop)) or (rop.refs > 1) then
    q_init(rop);
  result := rop.ptr;
end;

function dest(var rop: MPRandState): randstate_ptr;
begin
  if (not assigned(rop)) or (rop.refs > 1) then
    randinit_default(rop);
  result := rop.ptr;
end;

function src(var rop: MPInteger): mpz_ptr;
begin
  if not assigned(rop) then
    z_init(rop);
  result := rop.ptr;
end;

function src(var rop: MPFloat): mpf_ptr;
begin
  if not assigned(rop) then
    f_init(rop);
  result := rop.ptr;
end;

function src(var rop: MPRational): mpq_ptr;
begin
  if not assigned(rop) then
    q_init(rop);
  result := rop.ptr;
end;

function src(var rop: MPRandState): randstate_ptr;
begin
  if not assigned(rop) then
    randinit_default(rop);
  result := rop.ptr;
end;

procedure propagate_prec(var result, op: MPFloat);
begin
  f_set_prec(result, f_get_prec(op));
end;

procedure propagate_prec(var result, op1, op2: MPFloat);
begin
  f_set_prec(result, max(valsint(f_get_prec(op1)), f_get_prec(op2)));
end;

// --- ext bindings ----

procedure randinit_default(out state: MPRandState);
begin
  state := TMPRandState.Create;
  mp_randinit_default(state.ptr^);
end;

procedure randinit_lc_2exp(out state: MPRandState; var a: MPInteger; c, m2exp: valuint);
begin
  state := TMPRandState.Create;
  mp_randinit_lc_2exp(state.ptr^, src(a)^, c, m2exp);
end;

function randinit_lc_2exp_size(out state: MPRandState; size: sizeuint): boolean;
begin
  state := TMPRandState.Create;
  result := mp_randinit_lc_2exp_size(state.ptr^, size) <> 0;
end;

procedure randinit_mt(out state: MPRandState);
begin
  state := TMPRandState.Create;
  mp_randinit_mt(state.ptr^);
end;

procedure randinit_set(out rop: MPRandState; var op: MPRandState);
begin
  rop := TMPRandState.Create;
  mp_randinit_set(rop.ptr^, src(op)^);
end;

procedure randseed(var state: MPRandState; var seed: MPInteger);
begin
  mp_randseed(dest(state)^, src(seed)^);
end;

procedure randseed_ui(var state: MPRandState; seed: valuint);
begin
  mp_randseed_ui(dest(state)^, seed);
end;

procedure randclear(var state: MPRandState);
begin
  state := nil;
end;

function urandomb_ui(var state: MPRandState; n: valuint): valuint;
begin
  result := mp_urandomb_ui(dest(state)^, n);
end;

function urandomm_ui(var state: MPRandState; n: valuint): valuint;
begin
  result := mp_urandomb_ui(dest(state)^, n);
end;

function z_realloc(var integer_: MPInteger; new_alloc: mp_size_t): pointer;
begin
  result := mpz_realloc(dest(integer_)^, new_alloc);
end;

procedure z_abs(var rop, op: MPInteger);
begin
  mpz_abs(dest(rop)^, src(op)^);
end;

function z_abs(var op: MPInteger): MPInteger;
begin
  mpz_abs(dest(result)^, src(op)^);
end;

procedure z_add(var rop, op1, op2: MPInteger);
begin
  mpz_add(dest(rop)^, src(op1)^, src(op2)^);
end;

function z_add(var op1, op2: MPInteger): MPInteger;
begin
  mpz_add(dest(result)^, src(op1)^, src(op2)^);
end;

procedure z_add_ui(var rop, op1: MPInteger; op2: valuint);
begin
  mpz_add_ui(dest(rop)^, src(op1)^, op2);
end;

function z_add_ui(var op1: MPInteger; op2: valuint): MPInteger;
begin
  mpz_add_ui(dest(result)^, src(op1)^, op2);
end;

procedure z_addmul(var rop, op1, op2: MPInteger);
begin
  mpz_addmul(dest(rop)^, src(op1)^, src(op2)^);
end;

procedure z_addmul_ui(var rop, op1: MPInteger; op2: valuint);
begin
  mpz_addmul_ui(dest(rop)^, src(op1)^, op2);
end;

procedure z_and(var rop, op1, op2: MPInteger);
begin
  mpz_and(dest(rop)^, src(op1)^, src(op2)^);
end;

function z_and(var op1, op2: MPInteger): MPInteger;
begin
  mpz_and(dest(result)^, src(op1)^, src(op2)^);
end;

procedure z_bin_ui(var rop, n: MPInteger; k: valuint);
begin
  mpz_bin_ui(dest(rop)^, src(n)^, k);
end;

function z_bin_ui(var n: MPInteger; k: valuint): MPInteger;
begin
  mpz_bin_ui(dest(result)^, src(n)^, k);
end;

procedure z_bin_uiui(var rop: MPInteger; n, k: valuint);
begin
  mpz_bin_uiui(dest(rop)^, n, k);
end;

function z_bin_uiui(n, k: valuint): MPInteger;
begin
  mpz_bin_uiui(dest(result)^, n, k);
end;

procedure z_cdiv_q(var q, n, d: MPInteger);
begin
  mpz_cdiv_q(dest(q)^, src(n)^, src(d)^);
end;

function z_cdiv_q(var n, d: MPInteger): MPInteger;
begin
  mpz_cdiv_q(dest(result)^, src(n)^, src(d)^);
end;

procedure z_cdiv_q_2exp(var q, n: MPInteger; b: valuint);
begin
  mpz_cdiv_q_2exp(dest(q)^, src(n)^, b);
end;

function z_cdiv_q_2exp(var n: MPInteger; b: valuint): MPInteger;
begin
  mpz_cdiv_q_2exp(dest(result)^, src(n)^, b);
end;

function z_cdiv_q_ui(var q, n: MPInteger; d: valuint): valuint;
begin
  result := mpz_cdiv_q_ui(dest(q)^, src(n)^, d);
end;

procedure z_cdiv_qr(var q, r, n, d: MPInteger);
begin
  mpz_cdiv_qr(dest(q)^, dest(r)^, src(n)^, src(d)^);
end;

function z_cdiv_qr_ui(var q, r, n: MPInteger; d: valuint): valuint;
begin
  result := mpz_cdiv_qr_ui(dest(q)^, dest(r)^, src(n)^, d);
end;

procedure z_cdiv_r(var r, n, d: MPInteger);
begin
  mpz_cdiv_r(dest(r)^, src(n)^, src(d)^);
end;

function z_cdiv_r(var n, d: MPInteger): MPInteger;
begin
  mpz_cdiv_r(dest(result)^, src(n)^, src(d)^);
end;

procedure z_cdiv_r_2exp(var r, n: MPInteger; b: valuint);
begin
  mpz_cdiv_r_2exp(dest(r)^, src(n)^, b);
end;

function z_cdiv_r_2exp(var n: MPInteger; b: valuint): MPInteger;
begin
  mpz_cdiv_r_2exp(dest(result)^, src(n)^, b);
end;

function z_cdiv_r_ui(var r, n: MPInteger; d: valuint): valuint;
begin
  result := mpz_cdiv_r_ui(dest(r)^, src(n)^, d);
end;

function z_cdiv_ui(var n: MPInteger; d: valuint): valuint;
begin
  result := mpz_cdiv_ui(src(n)^, d);
end;

procedure z_clear(var integer_: MPInteger);
begin
  integer_ := nil;
end;

procedure z_clrbit(var rop: MPInteger; bit_index: valuint);
begin
  mpz_clrbit(dest(rop)^, bit_index);
end;

function z_cmp(var op1, op2: MPInteger): longint;
begin
  result := mpz_cmp(src(op1)^, src(op2)^);
end;

function z_cmp_d(var op1: MPInteger; op2: double): longint;
begin
  result := mpz_cmp_d(src(op1)^, op2);
end;

function z_cmp_si(var op1: MPInteger; op2: valsint): longint;
begin
  result := mpz_cmp_si(src(op1)^, op2);
end;

function z_cmp_ui(var op1: MPInteger; op2: valuint): longint;
begin
  result := mpz_cmp_ui(src(op1)^, op2);
end;

function z_cmpabs(var op1, op2: MPInteger): longint;
begin
  result := mpz_cmpabs(src(op1)^, src(op2)^);
end;

function z_cmpabs_d(var op1: MPInteger; op2: double): longint;
begin
  result := mpz_cmpabs_d(src(op1)^, op2);
end;

function z_cmpabs_ui(var op1: MPInteger; op2: valuint): longint;
begin
  result := mpz_cmpabs_ui(src(op1)^, op2);
end;

procedure z_com(var rop, op: MPInteger);
begin
  mpz_com(dest(rop)^, src(op)^);
end;

function z_com(var op: MPInteger): MPInteger;
begin
  mpz_com(dest(result)^, src(op)^);
end;

procedure z_combit(var rop: MPInteger; bit_index: valuint);
begin
  mpz_combit(dest(rop)^, bit_index);
end;

function z_congruent_p(var n, c, d: MPInteger): boolean;
begin
  result := mpz_congruent_p(src(n)^, src(c)^, src(d)^) <> 0;
end;

function z_congruent_2exp_p(var n, c: MPInteger; b: valuint): boolean;
begin
  result := mpz_congruent_2exp_p(src(n)^, src(c)^, b) <> 0;
end;

function z_congruent_ui_p(var n: MPInteger; c, d: valuint): boolean;
begin
  result := mpz_congruent_ui_p(src(n)^, c, d) <> 0;
end;

procedure z_divexact(var q, n, d: MPInteger);
begin
  mpz_divexact(dest(q)^, src(n)^, src(d)^);
end;

function z_divexact(var n, d: MPInteger): MPInteger;
begin
  mpz_divexact(dest(result)^, src(n)^, src(d)^);
end;

procedure z_divexact_ui(var q, n: MPInteger; d: valuint);
begin
  mpz_divexact_ui(dest(q)^, src(n)^, d);
end;

function z_divexact_ui(var n: MPInteger; d: valuint): MPInteger;
begin
  mpz_divexact_ui(dest(result)^, src(n)^, d);
end;

function z_divisible_p(var n, d: MPInteger): boolean;
begin
  result := mpz_divisible_p(src(n)^, src(d)^) <> 0;
end;

function z_divisible_ui_p(var n: MPInteger; d: valuint): boolean;
begin
  result := mpz_divisible_ui_p(src(n)^, d) <> 0;
end;

function z_divisible_2exp_p(var n: MPInteger; b: valuint): boolean;
begin
  result := mpz_divisible_2exp_p(src(n)^, b) <> 0;
end;

function z_export(out buf; out countp: sizeuint; order: longint; size: sizeuint; endian: longint; nails: sizeuint; var op: MPInteger): pointer;
begin
  result := mpz_export(buf, countp, order, size, endian, nails, src(op)^);
end;

procedure z_fac_ui(var rop: MPInteger; op: valuint);
begin
  mpz_fac_ui(dest(rop)^, op);
end;

function z_fac_ui(op: valuint): MPInteger;
begin
  mpz_fac_ui(dest(result)^, op);
end;

procedure z_fdiv_q(var q, n, d: MPInteger);
begin
  mpz_fdiv_q(dest(q)^, src(n)^, src(d)^);
end;

function z_fdiv_q(var n, d: MPInteger): MPInteger;
begin
  mpz_fdiv_q(dest(result)^, src(n)^, src(d)^);
end;

procedure z_fdiv_q_2exp(var q, n: MPInteger; b: valuint);
begin
  mpz_fdiv_q_2exp(dest(q)^, src(n)^, b);
end;

function z_fdiv_q_2exp(var n: MPInteger; b: valuint): MPInteger;
begin
  mpz_fdiv_q_2exp(dest(result)^, src(n)^, b);
end;

function z_fdiv_q_ui(var q, n: MPInteger; d: valuint): valuint;
begin
  result := mpz_fdiv_q_ui(dest(q)^, src(n)^, d);
end;

procedure z_fdiv_qr(var q, r, n, d: MPInteger);
begin
  mpz_fdiv_qr(dest(q)^, dest(r)^, src(n)^, src(d)^);
end;

function z_fdiv_qr_ui(var q, r, n: MPInteger; d: valuint): valuint;
begin
  result := mpz_fdiv_qr_ui(dest(q)^, dest(r)^, src(n)^, d);
end;

procedure z_fdiv_r(var r, n, d: MPInteger);
begin
  mpz_fdiv_r(dest(r)^, src(n)^, src(d)^);
end;

function z_fdiv_r(var n, d: MPInteger): MPInteger;
begin
  mpz_fdiv_r(dest(result)^, src(n)^, src(d)^);
end;

procedure z_fdiv_r_2exp(var r, n: MPInteger; b: valuint);
begin
  mpz_fdiv_r_2exp(dest(r)^, src(n)^, b);
end;

function z_fdiv_r_2exp(var n: MPInteger; b: valuint): MPInteger;
begin
  mpz_fdiv_r_2exp(dest(result)^, src(n)^, b);
end;

function z_fdiv_r_ui(var r, n: MPInteger; d: valuint): valuint;
begin
  result := mpz_fdiv_r_ui(dest(r)^, src(n)^, d);
end;

function z_fdiv_ui(var n: MPInteger; d: valuint): valuint;
begin
  result := mpz_fdiv_ui(src(n)^, d);
end;

procedure z_fib_ui(var fn: MPInteger; n: valuint);
begin
  mpz_fib_ui(dest(fn)^, n);
end;

function z_fib_ui(n: valuint): MPInteger;
begin
  mpz_fib_ui(dest(result)^, n);
end;

procedure z_fib2_ui(var fn, fnsub1: MPInteger; n: valuint);
begin
  mpz_fib2_ui(dest(fn)^, dest(fnsub1)^, n);
end;

function z_fib2_ui(var fnsub1: MPInteger; n: valuint): MPInteger;
begin
  mpz_fib2_ui(dest(result)^, dest(fnsub1)^, n);
end;

function z_fits_sint_p(var op: MPInteger): boolean;
begin
  result := mpz_fits_sint_p(src(op)^) <> 0;
end;

function z_fits_slong_p(var op: MPInteger): boolean;
begin
  result := mpz_fits_slong_p(src(op)^) <> 0;
end;

function z_fits_sshort_p(var op: MPInteger): boolean;
begin
  result := mpz_fits_sshort_p(src(op)^) <> 0;
end;

function z_fits_uint_p(var op: MPInteger): boolean;
begin
  result := mpz_fits_uint_p(src(op)^) <> 0;
end;

function z_fits_ulong_p(var op: MPInteger): boolean;
begin
  result := mpz_fits_ulong_p(src(op)^) <> 0;
end;

function z_fits_ushort_p(var op: MPInteger): boolean;
begin
  result := mpz_fits_ushort_p(src(op)^) <> 0;
end;

procedure z_gcd(var rop, op1, op2: MPInteger);
begin
  mpz_gcd(dest(rop)^, src(op1)^, src(op2)^);
end;

function z_gcd(var op1, op2: MPInteger): MPInteger;
begin
  mpz_gcd(dest(result)^, src(op1)^, src(op2)^);
end;

function z_gcd_ui(var rop, op1: MPInteger; op2: valuint): valuint;
begin
  result := mpz_gcd_ui(dest(rop)^, src(op1)^, op2)
end;

procedure z_gcdext(var g, s, t, a, b: MPInteger);
begin
  mpz_gcdext(dest(g)^, dest(s)^, dest(t)^, src(a)^, src(b)^);
end;

function z_get_d(var op: MPInteger): double;
begin
  result := mpz_get_d(src(op)^);
end;

function z_get_d_2exp(out exp: valsint; var op: MPInteger): double;
begin
  result := mpz_get_d_2exp(exp, src(op)^);
end;

function z_get_si(op: MPInteger): valsint;
begin
  result := mpz_get_si(src(op)^);
end;

function z_get_str(base: longint; var op: MPInteger): string;
var p: pchar;
begin
  p := mpz_get_str(nil, base, src(op)^);
  result := p;
  freemem(p);
end;

function z_get_str(str: pchar; base: longint; var op: MPInteger): pchar;
begin
  result := mpz_get_str(str, base, src(op)^);
end;

function z_get_ui(op: MPInteger): valuint;
begin
  result := mpz_get_ui(src(op)^);
end;

function z_getlimbn(var op: MPInteger; n: mp_size_t): mp_limb_t;
begin
  result := mpz_getlimbn(src(op)^, n);
end;

function z_hamdist(var op1, op2: MPInteger): valuint;
begin
  result := mpz_hamdist(src(op1)^, src(op2)^);
end;

procedure z_import(var rop: MPInteger; count: sizeuint; order: longint; size: sizeuint; endian: longint; nails: sizeuint; const op);
begin
  mpz_import(dest(rop)^, count, order, size, endian, nails, op);
end;

procedure z_init(out integer_: MPInteger);
begin
  integer_ := TMPInteger.Create;
  mpz_init(integer_.ptr^);
end;

procedure z_init2(out integer_: MPInteger; n: valuint);
begin
  integer_ := TMPInteger.Create;
  mpz_init2(integer_.ptr^, n);
end;

procedure z_init_set(out rop: MPInteger; var op: MPInteger);
begin
  rop := TMPInteger.Create;
  mpz_init_set(rop.ptr^, src(op)^);
end;

procedure z_init_set_d(out rop: MPInteger; op: double);
begin
  rop := TMPInteger.Create;
  mpz_init_set_d(rop.ptr^, op);
end;

procedure z_init_set_si(out rop: MPInteger; op: valsint);
begin
  rop := TMPInteger.Create;
  mpz_init_set_si(rop.ptr^, op);
end;

function z_init_set_str(out rop: MPInteger; str: string; base: longint): boolean;
begin
  rop := TMPInteger.Create;
  result := mpz_set_str(rop.ptr^, pchar(str), base) = 0;
end;

procedure z_init_set_ui(out rop: MPInteger; op: valuint);
begin
  rop := TMPInteger.Create;
  mpz_init_set_ui(rop.ptr^, op);
end;

function z_invert(var rop, op1, op2: MPInteger): longint;
begin
  result := mpz_invert(dest(rop)^, src(op1)^, src(op2)^);
end;

procedure z_ior(var rop, op1, op2: MPInteger);
begin
  mpz_ior(dest(rop)^, src(op1)^, src(op2)^);
end;

function z_ior(var op1, op2: MPInteger): MPInteger;
begin
  mpz_ior(dest(result)^, src(op1)^, src(op2)^);
end;

function z_jacobi(var a, b: MPInteger): longint;
begin
  result := mpz_jacobi(src(a)^, src(b)^);
end;

function z_kronecker_si(var a: MPInteger; b: valsint): longint;
begin
  result := mpz_kronecker_si(src(a)^, b);
end;

function z_kronecker_ui(var a: MPInteger; b: valuint): longint;
begin
  result := mpz_kronecker_ui(src(a)^, b);
end;

function z_si_kronecker(a: valsint; var b: MPInteger): longint;
begin
  result := mpz_si_kronecker(a, src(b)^);
end;

function z_ui_kronecker(a: valuint; var b: MPInteger): longint;
begin
  result := mpz_ui_kronecker(a, src(b)^);
end;

procedure z_lcm(var rop, op1, op2: MPInteger);
begin
  mpz_lcm(dest(rop)^, src(op1)^, src(op2)^);
end;

function z_lcm(var op1, op2: MPInteger): MPInteger;
begin
  mpz_lcm(dest(result)^, src(op1)^, src(op2)^);
end;

procedure z_lcm_ui(var rop, op1: MPInteger; op2: valuint);
begin
  mpz_lcm_ui(dest(rop)^, src(op1)^, op2);
end;

function z_lcm_ui(var op1: MPInteger; op2: valuint): MPInteger;
begin
  mpz_lcm_ui(dest(result)^, src(op1)^, op2);
end;

procedure z_lucnum_ui(var ln: MPInteger; n: valuint);
begin
  mpz_lucnum_ui(dest(ln)^, n);
end;

function z_lucnum_ui(n: valuint): MPInteger;
begin
  mpz_lucnum_ui(dest(result)^, n);
end;

procedure z_lucnum2_ui(var ln, lnsub1: MPInteger; n: valuint);
begin
  mpz_lucnum2_ui(dest(ln)^, dest(lnsub1)^, n);
end;

function z_lucnum2_ui(var lnsub1: MPInteger; n: valuint): MPInteger;
begin
  mpz_lucnum2_ui(dest(result)^, dest(lnsub1)^, n);
end;

procedure z_mod(var r, n, d: MPInteger);
begin
  mpz_mod(dest(r)^, src(n)^, src(d)^);
end;

function z_mod(var n, d: MPInteger): MPInteger;
begin
  mpz_mod(dest(result)^, src(n)^, src(d)^);
end;

procedure z_mul(var rop, op1, op2: MPInteger);
begin
  mpz_mul(dest(rop)^, src(op1)^, src(op2)^);
end;

function z_mul(var op1, op2: MPInteger): MPInteger;
begin
  mpz_mul(dest(result)^, src(op1)^, src(op2)^);
end;

procedure z_mul_2exp(var rop, op1: MPInteger; op2: valuint);
begin
  mpz_mul_2exp(dest(rop)^, src(op1)^, op2);
end;

function z_mul_2exp(var op1: MPInteger; op2: valuint): MPInteger;
begin
  mpz_mul_2exp(dest(result)^, src(op1)^, op2);
end;

procedure z_mul_si(var rop, op1: MPInteger; op2: valsint);
begin
  mpz_mul_si(dest(rop)^, src(op1)^, op2);
end;

function z_mul_si(var op1: MPInteger; op2: valsint): MPInteger;
begin
  mpz_mul_si(dest(result)^, src(op1)^, op2);
end;

procedure z_mul_ui(var rop, op1: MPInteger; op2: valuint);
begin
  mpz_mul_ui(dest(rop)^, src(op1)^, op2);
end;

function z_mul_ui(var op1: MPInteger; op2: valuint): MPInteger;
begin
  mpz_mul_ui(dest(result)^, src(op1)^, op2);
end;

procedure z_neg(var rop, op: MPInteger);
begin
  mpz_neg(dest(rop)^, src(op)^);
end;

function z_neg(var op: MPInteger): MPInteger;
begin
  mpz_neg(dest(result)^, src(op)^);
end;

procedure z_nextprime(var rop, op: MPInteger);
begin
  mpz_nextprime(dest(rop)^, src(op)^);
end;

function z_nextprime(var op: MPInteger): MPInteger;
begin
  mpz_nextprime(dest(result)^, src(op)^);
end;

function z_perfect_power_p(var op: MPInteger): boolean;
begin
  result := mpz_perfect_power_p(src(op)^) <> 0;
end;

function z_perfect_square_p(var op: MPInteger): boolean;
begin
  result := mpz_perfect_square_p(src(op)^) <> 0;
end;

function z_popcount(var op: MPInteger): valuint;
begin
  result := mpz_popcount(src(op)^);
end;

procedure z_pow_ui(var rop, base: MPInteger; exp: valuint);
begin
  mpz_pow_ui(dest(rop)^, src(base)^, exp);
end;

function z_pow_ui(var base: MPInteger; exp: valuint): MPInteger;
begin
  mpz_pow_ui(dest(result)^, src(base)^, exp);
end;

procedure z_powm(var rop, base, exp, mod_: MPInteger);
begin
  mpz_powm(dest(rop)^, src(base)^, src(exp)^, src(mod_)^);
end;

function z_powm(var base, exp, mod_: MPInteger): MPInteger;
begin
  mpz_powm(dest(result)^, src(base)^, src(exp)^, src(mod_)^);
end;

procedure z_powm_ui(var rop, base: MPInteger; exp: valuint; var mod_: MPInteger);
begin
  mpz_powm_ui(dest(rop)^, src(base)^, exp, src(mod_)^);
end;

function z_powm_ui(var base: MPInteger; exp: valuint; var mod_: MPInteger): MPInteger;
begin
  mpz_powm_ui(dest(result)^, src(base)^, exp, src(mod_)^);
end;

function z_probab_prime_p(var n: MPInteger; reps: longint): longint;
begin
  result := mpz_probab_prime_p(src(n)^, reps);
end;

procedure z_realloc2(var integer_: MPInteger; n: valuint);
begin
  mpz_realloc2(dest(integer_)^, n);
end;

function z_remove(var rop, op, f: MPInteger): valuint;
begin
  result := mpz_remove(dest(rop)^, src(op)^, src(f)^);
end;

function z_root(var rop, op: MPInteger; n: valuint): boolean;
begin
  result := mpz_root(dest(rop)^, src(op)^, n) <> 0;
end;

procedure z_rootrem(var root, rem, u: MPInteger; n: valuint);
begin
  mpz_rootrem(dest(root)^, dest(rem)^, src(u)^, n);
end;

procedure z_rrandomb(var rop: MPInteger; var state: MPRandState; n: valuint);
begin
  mpz_rrandomb(dest(rop)^, src(state)^, n);
end;

function z_rrandomb(var state: MPRandState; n: valuint): MPInteger;
begin
  mpz_rrandomb(dest(result)^, src(state)^, n);
end;

function z_scan0(var op: MPInteger; starting_bit: valuint): valuint;
begin
  result := mpz_scan0(src(op)^, starting_bit);
end;

function z_scan1(var op: MPInteger; starting_bit: valuint): valuint;
begin
  result := mpz_scan1(src(op)^, starting_bit);
end;

procedure z_set(var rop, op: MPInteger);
begin
  mpz_set(dest(rop)^, src(op)^);
end;

procedure z_set_d(var rop: MPInteger; op: double);
begin
  mpz_set_d(dest(rop)^, op);
end;

procedure z_set_f(var rop: MPInteger; var op: MPFloat);
begin
  mpz_set_f(dest(rop)^, src(op)^);
end;

procedure z_set_q(var rop: MPInteger; var op: MPRational);
begin
  mpz_set_q(dest(rop)^, src(op)^);
end;

procedure z_set_si(var rop: MPInteger; op: valsint);
begin
  mpz_set_si(dest(rop)^, op);
end;

function z_set_str(var rop: MPInteger; str: string; base: longint): boolean;
begin
  result := mpz_set_str(dest(rop)^, pchar(str), base) = 0;
end;

procedure z_set_ui(var rop: MPInteger; op: valuint);
begin
  mpz_set_ui(dest(rop)^, op);
end;

procedure z_setbit(var rop: MPInteger; bit_index: valuint);
begin
  mpz_setbit(dest(rop)^, bit_index);
end;

function z_size(var op: MPInteger): sizeuint;
begin
  result := mpz_size(src(op)^);
end;

function z_sizeinbase(var op: MPInteger; base: longint): sizeuint;
begin
  result := mpz_sizeinbase(src(op)^, base);
end;

procedure z_sqrt(var rop, op: MPInteger);
begin
  mpz_sqrt(dest(rop)^, src(op)^);
end;

function z_sqrt(var op: MPInteger): MPInteger;
begin
  mpz_sqrt(dest(result)^, src(op)^);
end;

procedure z_sqrtrem(var rop1, rop2, op: MPInteger);
begin
  mpz_sqrtrem(dest(rop1)^, dest(rop2)^, src(op)^);
end;

procedure z_sub(var rop, op1, op2: MPInteger);
begin
  mpz_sub(dest(rop)^, src(op1)^, src(op2)^);
end;

function z_sub(var op1, op2: MPInteger): MPInteger;
begin
  mpz_sub(dest(result)^, src(op1)^, src(op2)^);
end;

procedure z_sub_ui(var rop, op1: MPInteger; op2: valuint);
begin
  mpz_sub_ui(dest(rop)^, src(op1)^, op2);
end;

function z_sub_ui(var op1: MPInteger; op2: valuint): MPInteger;
begin
  mpz_sub_ui(dest(result)^, src(op1)^, op2);
end;

procedure z_ui_sub(var rop: MPInteger; op1: valuint; var op2: MPInteger);
begin
  mpz_ui_sub(dest(rop)^, op1, src(op2)^);
end;

function z_ui_sub(op1: valuint; var op2: MPInteger): MPInteger;
begin
  mpz_ui_sub(dest(result)^, op1, src(op2)^);
end;

procedure z_submul(var rop, op1, op2: MPInteger);
begin
  mpz_submul(dest(rop)^, src(op1)^, src(op2)^);
end;

procedure z_submul_ui(var rop, op1: MPInteger; op2: valuint);
begin
  mpz_submul_ui(dest(rop)^, src(op1)^, op2);
end;

procedure z_swap(var rop1, rop2: MPInteger);
begin
  mpz_swap(dest(rop1)^, dest(rop2)^);
end;

procedure z_tdiv_q(var q, n, d: MPInteger);
begin
  mpz_tdiv_q(dest(q)^, src(n)^, src(d)^);
end;

function z_tdiv_q(var n, d: MPInteger): MPInteger;
begin
  mpz_tdiv_q(dest(result)^, src(n)^, src(d)^);
end;

procedure z_tdiv_q_2exp(var q, n: MPInteger; b: valuint);
begin
  mpz_tdiv_q_2exp(dest(q)^, src(n)^, b);
end;

function z_tdiv_q_2exp(var n: MPInteger; b: valuint): MPInteger;
begin
  mpz_tdiv_q_2exp(dest(result)^, src(n)^, b);
end;

function z_tdiv_q_ui(var q, n: MPInteger; d: valuint): valuint;
begin
  result := mpz_tdiv_q_ui(dest(q)^, src(n)^, d);
end;

procedure z_tdiv_qr(var q, r, n, d: MPInteger);
begin
  mpz_tdiv_qr(dest(q)^, dest(r)^, src(n)^, src(d)^);
end;

function z_tdiv_qr_ui(var q, r, n: MPInteger; d: valuint): valuint;
begin
  result := mpz_tdiv_qr_ui(dest(q)^, dest(r)^, src(n)^, d);
end;

procedure z_tdiv_r(var r, n, d: MPInteger);
begin
  mpz_tdiv_r(dest(r)^, src(n)^, src(d)^);
end;

function z_tdiv_r(var n, d: MPInteger): MPInteger;
begin
  mpz_tdiv_r(dest(result)^, src(n)^, src(d)^);
end;

procedure z_tdiv_r_2exp(var r, n: MPInteger; b: valuint);
begin
  mpz_tdiv_r_2exp(dest(r)^, src(n)^, b);
end;

function z_tdiv_r_2exp(var n: MPInteger; b: valuint): MPInteger;
begin
  mpz_tdiv_r_2exp(dest(result)^, src(n)^, b);
end;

function z_tdiv_r_ui(var r, n: MPInteger; d: valuint): valuint;
begin
  result := mpz_tdiv_r_ui(dest(r)^, src(n)^, d);
end;

function z_tdiv_ui(var n: MPInteger; d: valuint): valuint;
begin
  result := mpz_tdiv_ui(src(n)^, d);
end;

function z_tstbit(var rop: MPInteger; bit_index: valuint): boolean;
begin
  result := mpz_tstbit(src(rop)^, bit_index) <> 0;
end;

procedure z_ui_pow_ui(var rop: MPInteger; base, exp: valuint);
begin
  mpz_ui_pow_ui(dest(rop)^, base, exp);
end;

function z_ui_pow_ui(base, exp: valuint): MPInteger;
begin
  mpz_ui_pow_ui(dest(result)^, base, exp);
end;

procedure z_urandomb(var rop: MPInteger; var state: MPRandState; n: valuint);
begin
  mpz_urandomb(dest(rop)^, src(state)^, n);
end;

function z_urandomb(var state: MPRandState; n: valuint): MPInteger;
begin
  mpz_urandomb(dest(result)^, src(state)^, n);
end;

procedure z_urandomm(var rop: MPInteger; var state: MPRandState; var n: MPInteger);
begin
  mpz_urandomm(dest(rop)^, src(state)^, src(n)^);
end;

function z_urandomm(var state: MPRandState; var n: MPInteger): MPInteger;
begin
  mpz_urandomm(dest(result)^, src(state)^, src(n)^);
end;

procedure z_xor(var rop, op1, op2: MPInteger);
begin
  mpz_xor(dest(rop)^, src(op1)^, src(op2)^);
end;

function z_xor(var op1, op2: MPInteger): MPInteger;
begin
  mpz_xor(dest(result)^, src(op1)^, src(op2)^);
end;

procedure q_abs(var rop, op: MPRational);
begin
  mpq_abs(dest(rop)^, src(op)^);
end;

function q_abs(var op: MPRational): MPRational;
begin
  mpq_abs(dest(result)^, src(op)^);
end;

procedure q_add(var sum, addend1, addend2: MPRational);
begin
  mpq_add(dest(sum)^, src(addend1)^, src(addend2)^);
end;

function q_add(var addend1, addend2: MPRational): MPRational;
begin
  mpq_add(dest(result)^, src(addend1)^, src(addend2)^);
end;

procedure q_canonicalize(var op: MPRational);
begin
  mpq_canonicalize(dest(op)^);
end;

procedure q_clear(var rational_number: MPRational);
begin
  rational_number := nil;
end;

function q_cmp(var op1, op2: MPRational): longint;
begin
  result := mpq_cmp(src(op1)^, src(op2)^);
end;

function q_cmp_si(var op1: MPRational; num2: valsint; den2: valuint): longint;
begin
  result := mpq_cmp_si(src(op1)^, num2, den2);
end;

function q_cmp_ui(var op1: MPRational; num2, den2: valuint): longint;
begin
  result := mpq_cmp_ui(src(op1)^, num2, den2);
end;

procedure q_div(var quotient, dividend, divisor: MPRational);
begin
  mpq_div(dest(quotient)^, src(dividend)^, src(divisor)^);
end;

function q_div(var dividend, divisor: MPRational): MPRational;
begin
  mpq_div(dest(result)^, src(dividend)^, src(divisor)^);
end;

procedure q_div_2exp(var rop, op1: MPRational; op2: valuint);
begin
  mpq_div_2exp(dest(rop)^, src(op1)^, op2);
end;

function q_div_2exp(var op1: MPRational; op2: valuint): MPRational;
begin
  mpq_div_2exp(dest(result)^, src(op1)^, op2);
end;

function q_equal(var op1, op2: MPRational): boolean;
begin
  result := mpq_equal(src(op1)^, src(op2)^) <> 0;
end;

procedure q_get_num(var numerator: MPInteger; var rational: MPRational);
begin
  mpq_get_num(dest(numerator)^, src(rational)^);
end;

function q_get_num(var rational: MPRational): MPInteger;
begin
  mpq_get_num(dest(result)^, src(rational)^);
end;

procedure q_get_den(var denominator: MPInteger; var rational: MPRational);
begin
  mpq_get_den(dest(denominator)^, src(rational)^);
end;

function q_get_den(var rational: MPRational): MPInteger;
begin
  mpq_get_den(dest(result)^, src(rational)^);
end;

function q_get_d(var op: MPRational): double;
begin
  result := mpq_get_d(src(op)^);
end;

function q_get_str(base: longint; var op: MPRational): string;
var p: pchar;
begin
  p := mpq_get_str(nil, base, src(op)^);
  result := p;
  freemem(p);
end;

function q_get_str(str: pchar; base: longint; var op: MPRational): pchar;
begin
  result := mpq_get_str(str, base, src(op)^);
end;

procedure q_init(out dest_rational: MPRational);
begin
  dest_rational := TMPRational.Create;
  mpq_init(dest_rational.ptr^);
end;

procedure q_inv(var inverted_number, number: MPRational);
begin
  mpq_inv(dest(inverted_number)^, src(number)^);
end;

function q_inv(var number: MPRational): MPRational;
begin
  mpq_inv(dest(result)^, src(number)^);
end;

procedure q_mul(var product, multiplier, multiplicand: MPRational);
begin
  mpq_mul(dest(product)^, src(multiplier)^, src(multiplicand)^);
end;

function q_mul(var multiplier, multiplicand: MPRational): MPRational;
begin
  mpq_mul(dest(result)^, src(multiplier)^, src(multiplicand)^);
end;

procedure q_mul_2exp(var rop, op1: MPRational; op2: valuint);
begin
  mpq_mul_2exp(dest(rop)^, src(op1)^, op2);
end;

function q_mul_2exp(var op1: MPRational; op2: valuint): MPRational;
begin
  mpq_mul_2exp(dest(result)^, src(op1)^, op2);
end;

procedure q_neg(var negated_operand, operand: MPRational);
begin
  mpq_neg(dest(negated_operand)^, src(operand)^);
end;

function q_neg(var operand: MPRational): MPRational;
begin
  mpq_neg(dest(result)^, src(operand)^);
end;

procedure q_set(var rop, op: MPRational);
begin
  mpq_set(dest(rop)^, src(op)^);
end;

procedure q_set_d(var rop: MPRational; op: double);
begin
  mpq_set_d(dest(rop)^, op);
end;

procedure q_set_den(var rational: MPRational; var denominator: MPInteger);
begin
  mpq_set_den(dest(rational)^, src(denominator)^);
end;

procedure q_set_f(var rop: MPRational; var op: MPFloat);
begin
  mpq_set_f(dest(rop)^, src(op)^);
end;

procedure q_set_num(var rational: MPRational; var numerator: MPInteger);
begin
  mpq_set_num(dest(rational)^, src(numerator)^);
end;

procedure q_set_si(var rop: MPRational; op1: valsint; op2: valuint);
begin
  mpq_set_si(dest(rop)^, op1, op2);
end;

function q_set_str(var rop: MPRational; str: string; base: longint): boolean;
begin
  result := mpq_set_str(dest(rop)^, pchar(str), base) = 0;
end;

procedure q_set_ui(var rop: MPRational; op1, op2: valuint);
begin
  mpq_set_ui(dest(rop)^, op1, op2);
end;

procedure q_set_z(var rop: MPRational; var op: MPInteger);
begin
  mpq_set_z(dest(rop)^, src(op)^);
end;

procedure q_sub(var difference, minuend, subtrahend: MPRational);
begin
  mpq_sub(dest(difference)^, src(minuend)^, src(subtrahend)^);
end;

function q_sub(var minuend, subtrahend: MPRational): MPRational;
begin
  mpq_sub(dest(result)^, src(minuend)^, src(subtrahend)^);
end;

procedure q_swap(var rop1, rop2: MPRational);
begin
  mpq_swap(dest(rop1)^, dest(rop2)^);
end;

procedure f_abs(var rop, op: MPFloat);
begin
  mpf_abs(dest(rop)^, src(op)^);
end;

function f_abs(var op: MPFloat): MPFloat;
begin
  mpf_abs(dest(result)^, src(op)^);
end;

procedure f_add(var rop, op1, op2: MPFloat);
begin
  mpf_add(dest(rop)^, src(op1)^, src(op2)^);
end;

function f_add(var op1, op2: MPFloat): MPFloat;
begin
  mpf_add(dest(result)^, src(op1)^, src(op2)^);
end;

procedure f_add_ui(var rop, op1: MPFloat; op2: valuint);
begin
  mpf_add_ui(dest(rop)^, src(op1)^, op2);
end;

function f_add_ui(var op1: MPFloat; op2: valuint): MPFloat;
begin
  mpf_add_ui(dest(result)^, src(op1)^, op2);
end;

procedure f_ceil(var rop, op: MPFloat);
begin
  mpf_ceil(dest(rop)^, src(op)^);
end;

function f_ceil(var op: MPFloat): MPFloat;
begin
  mpf_ceil(dest(result)^, src(op)^);
end;

procedure f_clear(var x: MPFloat);
begin
  x := nil;
end;

function f_cmp(var op1, op2: MPFloat): longint;
begin
  result := mpf_cmp(src(op1)^, src(op2)^);
end;

function f_cmp_d(var op1: MPFloat; op2: double): longint;
begin
  result := mpf_cmp_d(src(op1)^, op2);
end;

function f_cmp_si(var op1: MPFloat; op2: valsint): longint;
begin
  result := mpf_cmp_si(src(op1)^, op2);
end;

function f_cmp_ui(var op1: MPFloat; op2: valuint): longint;
begin
  result := mpf_cmp_ui(src(op1)^, op2);
end;

procedure f_div(var rop, op1, op2: MPFloat);
begin
  mpf_div(dest(rop)^, src(op1)^, src(op2)^);
end;

function f_div(var op1, op2: MPFloat): MPFloat;
begin
  mpf_div(dest(result)^, src(op1)^, src(op2)^);
end;

procedure f_div_2exp(var rop, op1: MPFloat; op2: valuint);
begin
  mpf_div_2exp(dest(rop)^, src(op1)^, op2);
end;

function f_div_2exp(var op1: MPFloat; op2: valuint): MPFloat;
begin
  mpf_div_2exp(dest(result)^, src(op1)^, op2);
end;

procedure f_div_ui(var rop, op1: MPFloat; op2: valuint);
begin
  mpf_div_ui(dest(rop)^, src(op1)^, op2);
end;

function f_div_ui(var op1: MPFloat; op2: valuint): MPFloat;
begin
  mpf_div_ui(dest(result)^, src(op1)^, op2);
end;

function f_eq(var op1, op2: MPFloat; op3: valuint): boolean;
begin
  result := mpf_eq(src(op1)^, src(op2)^, op3) <> 0;
end;

function f_fits_sint_p(var op: MPFloat): boolean;
begin
  result := mpf_fits_sint_p(src(op)^) <> 0;
end;

function f_fits_slong_p(var op: MPFloat): boolean;
begin
  result := mpf_fits_slong_p(src(op)^) <> 0;
end;

function f_fits_sshort_p(var op: MPFloat): boolean;
begin
  result := mpf_fits_sshort_p(src(op)^) <> 0;
end;

function f_fits_uint_p(var op: MPFloat): boolean;
begin
  result := mpf_fits_uint_p(src(op)^) <> 0;
end;

function f_fits_ulong_p(var op: MPFloat): boolean;
begin
  result := mpf_fits_ulong_p(src(op)^) <> 0;
end;

function f_fits_ushort_p(var op: MPFloat): boolean;
begin
  result := mpf_fits_ushort_p(src(op)^) <> 0;
end;

procedure f_floor(var rop, op: MPFloat);
begin
  mpf_floor(dest(rop)^, src(op)^);
end;

function f_floor(var op: MPFloat): MPFloat;
begin
  mpf_floor(dest(result)^, src(op)^);
end;

function f_get_d(var op: MPFloat): double;
begin
  result := mpf_get_d(src(op)^);
end;

function f_get_d_2exp(out exp: valsint; var op: MPFloat): double;
begin
  result := mpf_get_d_2exp(exp, src(op)^);
end;

function f_get_default_prec: valuint;
begin
  result := mpf_get_default_prec;
end;

function f_get_prec(var op: MPFloat): valuint;
begin
  result := mpf_get_prec(src(op)^);
end;

function f_get_si(var op: MPFloat): valsint;
begin
  result := mpf_get_si(src(op)^);
end;

function f_get_str(out exp: mp_exp_t; base: longint; ndigits: sizeuint; var op: MPFloat): string;
var p: pchar;
begin
  p := mpf_get_str(nil, exp, base, ndigits, src(op)^);
  result := p;
  freemem(p);
end;

function f_get_str(str: pchar; out exp: mp_exp_t; base: longint; ndigits: sizeuint; var op: MPFloat): pchar;
begin
  result := mpf_get_str(str, exp, base, ndigits, src(op)^);
end;

function f_get_ui(var op: MPFloat): valuint;
begin
  result := mpf_get_ui(src(op)^);
end;

procedure f_init(out x: MPFloat);
begin
  x := TMPFloat.Create;
  mpf_init(x.ptr^);
end;

procedure f_init2(out x: MPFloat; prec: valuint);
begin
  x := TMPFloat.Create;
  mpf_init2(x.ptr^, prec);
end;

procedure f_init_set(out rop: MPFloat; var op: MPFloat);
begin
  rop := TMPFloat.Create;
  mpf_init_set(rop.ptr^, src(op)^);
end;

procedure f_init_set_d(out rop: MPFloat; op: double);
begin
  rop := TMPFloat.Create;
  mpf_init_set_d(rop.ptr^, op);
end;

procedure f_init_set_si(out rop: MPFloat; op: valsint);
begin
  rop := TMPFloat.Create;
  mpf_init_set_si(rop.ptr^, op);
end;

function f_init_set_str(out rop: MPFloat; str: string; base: longint): boolean;
begin
  rop := TMPFloat.Create;
  result := mpf_init_set_str(rop.ptr^, pchar(str), base) = 0;
end;

procedure f_init_set_ui(out rop: MPFloat; op: valuint);
begin
  rop := TMPFloat.Create;
  mpf_init_set_ui(rop.ptr^, op);
end;

function f_integer_p(var op: MPFloat): boolean;
begin
  result := mpf_integer_p(src(op)^) <> 0;
end;

procedure f_mul(var rop, op1, op2: MPFloat);
begin
  mpf_mul(dest(rop)^, src(op1)^, src(op2)^);
end;

function f_mul(var op1, op2: MPFloat): MPFloat;
begin
  mpf_mul(dest(result)^, src(op1)^, src(op2)^);
end;

procedure f_mul_2exp(var rop, op1: MPFloat; op2: valuint);
begin
  mpf_mul_2exp(dest(rop)^, src(op1)^, op2);
end;

function f_mul_2exp(var op1: MPFloat; op2: valuint): MPFloat;
begin
  mpf_mul_2exp(dest(result)^, src(op1)^, op2);
end;

procedure f_mul_ui(var rop, op1: MPFloat; op2: valuint);
begin
  mpf_mul_ui(dest(rop)^, src(op1)^, op2);
end;

function f_mul_ui(var op1: MPFloat; op2: valuint): MPFloat;
begin
  mpf_mul_ui(dest(result)^, src(op1)^, op2);
end;

procedure f_neg(var rop, op: MPFloat);
begin
  mpf_neg(dest(rop)^, src(op)^);
end;

function f_neg(var op: MPFloat): MPFloat;
begin
  mpf_neg(dest(result)^, src(op)^);
end;

procedure f_pow_ui(var rop, op1: MPFloat; op2: valuint);
begin
  mpf_pow_ui(dest(rop)^, src(op1)^, op2);
end;

function f_pow_ui(var op1: MPFloat; op2: valuint): MPFloat;
begin
  mpf_pow_ui(dest(result)^, src(op1)^, op2);
end;

procedure f_random2(var rop: MPFloat; max_size: mp_size_t; exp: mp_exp_t);
begin
  mpf_random2(dest(rop)^, max_size, exp);
end;

function f_random2(max_size: mp_size_t; exp: mp_exp_t): MPFloat;
begin
  mpf_random2(dest(result)^, max_size, exp);
end;

procedure f_reldiff(var rop, op1, op2: MPFloat);
begin
  mpf_reldiff(dest(rop)^, src(op1)^, src(op2)^);
end;

function f_reldiff(var op1, op2: MPFloat): MPFloat;
begin
  mpf_reldiff(dest(result)^, src(op1)^, src(op2)^);
end;

procedure f_set(var rop, op: MPFloat);
begin
  mpf_set(dest(rop)^, src(op)^);
end;

procedure f_set_d(var rop: MPFloat; op: double);
begin
  mpf_set_d(dest(rop)^, op);
end;

procedure f_set_default_prec(prec: valuint);
begin
  mpf_set_default_prec(prec);
end;

procedure f_set_prec(var rop: MPFloat; prec: valuint);
begin
  mpf_set_prec(dest(rop)^, prec);
end;

procedure f_set_prec_raw(var rop: MPFloat; prec: valuint);
begin
  mpf_set_prec_raw(dest(rop)^, prec);
end;

procedure f_set_q(var rop: MPFloat; var op: MPRational);
begin
  mpf_set_q(dest(rop)^, src(op)^);
end;

procedure f_set_si(var rop: MPFloat; op: valsint);
begin
  mpf_set_si(dest(rop)^, op);
end;

function f_set_str(var rop: MPFloat; str: string; base: longint): boolean;
begin
  result := mpf_set_str(dest(rop)^, pchar(str), base) = 0;
end;

procedure f_set_ui(var rop: MPFloat; op: valuint);
begin
  mpf_set_ui(dest(rop)^, op);
end;

procedure f_set_z(var rop: MPFloat; var op: MPInteger);
begin
  mpf_set_z(dest(rop)^, src(op)^);
end;

procedure f_sqrt(var rop, op: MPFloat);
begin
  mpf_sqrt(dest(rop)^, src(op)^);
end;

function f_sqrt(var op: MPFloat): MPFloat;
begin
  mpf_sqrt(dest(result)^, src(op)^);
end;

procedure f_sqrt_ui(var rop: MPFloat; op: valuint);
begin
  mpf_sqrt_ui(dest(rop)^, op);
end;

function f_sqrt_ui(op: valuint): MPFloat;
begin
  mpf_sqrt_ui(dest(result)^, op);
end;

procedure f_sub(var rop, op1, op2: MPFloat);
begin
  mpf_sub(dest(rop)^, src(op1)^, src(op2)^);
end;

function f_sub(var op1, op2: MPFloat): MPFloat;
begin
  mpf_sub(dest(result)^, src(op1)^, src(op2)^);
end;

procedure f_sub_ui(var rop, op1: MPFloat; op2: valuint);
begin
  mpf_sub_ui(dest(rop)^, src(op1)^, op2);
end;

function f_sub_ui(var op1: MPFloat; op2: valuint): MPFloat;
begin
  mpf_sub_ui(dest(result)^, src(op1)^, op2);
end;

procedure f_swap(var rop1, rop2: MPFloat);
begin
  mpf_swap(dest(rop1)^, dest(rop2)^);
end;

procedure f_trunc(var rop, op: MPFloat);
begin
  mpf_trunc(dest(rop)^, src(op)^);
end;

function f_trunc(var op: MPFloat): MPFloat;
begin
  mpf_trunc(dest(result)^, src(op)^);
end;

procedure f_ui_div(var rop: MPFloat; op1: valuint; var op2: MPFloat);
begin
  mpf_ui_div(dest(rop)^, op1, src(op2)^);
end;

function f_ui_div(op1: valuint; var op2: MPFloat): MPFloat;
begin
  mpf_ui_div(dest(result)^, op1, src(op2)^);
end;

procedure f_ui_sub(var rop: MPFloat; op1: valuint; var op2: MPFloat);
begin
  mpf_ui_sub(dest(rop)^, op1, src(op2)^);
end;

function f_ui_sub(op1: valuint; var op2: MPFloat): MPFloat;
begin
  mpf_ui_sub(dest(result)^, op1, src(op2)^);
end;

procedure f_urandomb(var rop: MPFloat; var state: MPRandState; nbits: valuint);
begin
  mpf_urandomb(dest(rop)^, src(state)^, nbits);
end;

function f_urandomb(var state: MPRandState; nbits: valuint): MPFloat;
begin
  mpf_urandomb(dest(result)^, src(state)^, nbits);
end;

// ---- operators ----

operator * (op1: MPFloat; op2: MPFloat): MPFloat;
begin
  propagate_prec(result, op1, op2);
  f_mul(result, op1, op2);
end;

operator * (op1: MPInteger; op2: MPInteger): MPInteger;
begin
  z_mul(result, op1, op2);
end;

operator * (op1: MPRational; op2: MPRational): MPRational;
begin
  q_mul(result, op1, op2);
end;

operator ** (op1: MPFloat; op2: valuint): MPFloat;
begin
  propagate_prec(result, op1);
  f_pow_ui(result, op1, op2);
end;

operator ** (op1: MPInteger; op2: valuint): MPInteger;
begin
  z_pow_ui(result, op1, op2);
end;

operator + (op1: MPFloat; op2: MPFloat): MPFloat;
begin
  propagate_prec(result, op1, op2);
  f_add(result, op1, op2);
end;

operator + (op1: MPInteger; op2: MPInteger): MPInteger;
begin
  z_add(result, op1, op2);
end;

operator + (op1: MPRational; op2: MPRational): MPRational;
begin
  q_add(result, op1, op2);
end;

operator - (op: MPFloat): MPFloat;
begin
  propagate_prec(result, op);
  f_neg(result, op);
end;

operator - (op: MPInteger): MPInteger;
begin
  z_neg(result, op);
end;

operator - (op: MPRational): MPRational;
begin
  q_neg(result, op);
end;

operator - (op1: MPFloat; op2: MPFloat): MPFloat;
begin
  propagate_prec(result, op1, op2);
  f_sub(result, op1, op2);
end;

operator - (op1: MPInteger; op2: MPInteger): MPInteger;
begin
  z_sub(result, op1, op2);
end;

operator - (op1: MPRational; op2: MPRational): MPRational;
begin
  q_sub(result, op1, op2);
end;

operator / (op1: MPFloat; op2: MPFloat): MPFloat;
begin
  propagate_prec(result, op1, op2);
  f_div(result, op1, op2);
end;

operator / (op1: MPInteger; op2: MPInteger): MPInteger;
begin
  z_tdiv_q(result, op1, op2);
end;

operator / (op1: MPRational; op2: MPRational): MPRational;
begin
  q_div(result, op1, op2);
end;

operator := (op: double): MPFloat;
begin
  f_set_d(result, op);
end;

operator := (op: double): MPInteger;
begin
  z_set_d(result, op);
end;

operator := (op: double): MPRational;
begin
  q_set_d(result, op);
end;

operator := (op: MPFloat): cardinal;
begin
  result := f_get_ui(op);
end;

operator := (op: MPFloat): double;
begin
  result := f_get_d(op);
end;

operator := (op: MPFloat): integer;
begin
  result := f_get_si(op);
end;

operator := (op: MPFloat): mpf_t;
begin
  mpf_init_set(result, src(op)^);
end;

operator := (op: MPFloat): MPInteger;
begin
  z_set_f(result, op);
end;

operator := (op: MPFloat): MPRational;
begin
  q_set_f(result, op);
end;

operator := (op: MPFloat): string;
const FMT = '%.*Fg';
var p: pchar;
begin
  mp_asprintf(p, FMT, floor(f_get_prec(op) * LOG_10_2), src(op));
  result := p;
  freemem(p);
end;

{$ifdef CPU64}
operator := (op: MPFloat): valsint;
begin
  result := f_get_si(op);
end;

operator := (op: MPFloat): valuint;
begin
  result := f_get_ui(op);
end;
{$endif}

operator := (var op: mpf_t): MPFloat;
begin
  mpf_set(dest(result)^, op);
end;

operator := (op: MPInteger): cardinal;
begin
  result := z_get_ui(op);
end;

operator := (op: MPInteger): double;
begin
  result := z_get_d(op);
end;

operator := (op: MPInteger): integer;
begin
  result := z_get_si(op);
end;

operator := (op: MPInteger): MPFloat;
begin
  f_set_z(result, op);
end;

operator := (op: MPInteger): MPRational;
begin
  q_set_z(result, op);
end;

operator := (op: MPInteger): mpz_t;
begin
  mpz_init_set(result, src(op)^);
end;

operator := (op: MPInteger): string;
const FMT = '%Zd';
var p: pchar;
begin
  mp_asprintf(p, FMT, src(op));
  result := p;
  freemem(p);
end;

{$ifdef CPU64}
operator := (op: MPInteger): valsint;
begin
  result := z_get_si(op);
end;

operator := (op: MPInteger): valuint;
begin
  result := z_get_ui(op);
end;
{$endif}

operator := (var op: mpq_t): MPRational;
begin
  mpq_set(dest(result)^, op);
end;

operator := (op: MPRandState): randstate_t;
begin
  mp_randinit_set(result, src(op)^);
end;

operator := (op: MPRational): double;
begin
  result := q_get_d(op);
end;

operator := (op: MPRational): MPFloat;
begin
  f_set_q(result, op);
end;

operator := (op: MPRational): MPInteger;
begin
  z_set_q(result, op);
end;

operator := (op: MPRational): mpq_t;
begin
  mpq_init(result);
  mpq_set(result, src(op)^);
end;

operator := (op: MPRational): string;
const FMT = '%Qd';
var p: pchar;
begin
  mp_asprintf(p, FMT, src(op));
  result := p;
  freemem(p);
end;

operator := (var op: mpz_t): MPInteger;
begin
  mpz_set(dest(result)^, op);
end;

operator := (var op: randstate_t): MPRandState;
begin
  result := TMPRandState.Create;
  mp_randinit_set(result.ptr^, op);
end;

operator := (op: string): MPFloat;
begin
  f_set_prec(result, ceil(length(op) / LOG_10_2));
  f_set_str(result, op, BASE10);
end;

operator := (op: string): MPInteger;
begin
  z_set_str(result, op, BASE10);
end;

operator := (op: string): MPRational;
begin
  q_set_str(result, op, BASE10);
end;

operator := (op: valsint): MPFloat;
begin
  f_set_si(result, op);
end;

operator := (op: valsint): MPInteger;
begin
  z_set_si(result, op);
end;

operator := (op: valsint): MPRational;
begin
  q_set_si(result, op, 1);
end;

operator := (op: valuint): MPFloat;
begin
  f_set_ui(result, op);
end;

operator := (op: valuint): MPInteger;
begin
  z_set_ui(result, op);
end;

operator := (op: valuint): MPRational;
begin
  q_set_ui(result, op, 1);
end;

operator < (op1: MPFloat; op2: MPFloat): boolean;
begin
  result := f_cmp(op1, op2) < 0;
end;

operator < (op1: MPInteger; op2: MPInteger): boolean;
begin
  result := z_cmp(op1, op2) < 0;
end;

operator < (op1: MPRational; op2: MPRational): boolean;
begin
  result := q_cmp(op1, op2) < 0;
end;

operator <= (op1: MPFloat; op2: MPFloat): boolean;
begin
  result := f_cmp(op1, op2) <= 0;
end;

operator <= (op1: MPInteger; op2: MPInteger): boolean;
begin
  result := z_cmp(op1, op2) <= 0;
end;

operator <= (op1: MPRational; op2: MPRational): boolean;
begin
  result := q_cmp(op1, op2) <= 0;
end;

//operator = (op1: MPFloat; op2: MPFloat): boolean;
//begin
//  result := f_cmp(op1, op2) = 0;
//end;

//operator = (op1: MPInteger; op2: MPInteger): boolean;
//begin
//  result := z_cmp(op1, op2) = 0;
//end;

//operator = (op1: MPRational; op2: MPRational): boolean;
//begin
//  result := q_cmp(op1, op2) = 0;
//end;

operator > (op1: MPFloat; op2: MPFloat): boolean;
begin
  result := f_cmp(op1, op2) > 0;
end;

operator > (op1: MPInteger; op2: MPInteger): boolean;
begin
  result := z_cmp(op1, op2) > 0;
end;

operator > (op1: MPRational; op2: MPRational): boolean;
begin
  result := q_cmp(op1, op2) > 0;
end;

operator >= (op1: MPFloat; op2: MPFloat): boolean;
begin
  result := f_cmp(op1, op2) >= 0;
end;

operator >= (op1: MPInteger; op2: MPInteger): boolean;
begin
  result := z_cmp(op1, op2) >= 0;
end;

operator >= (op1: MPRational; op2: MPRational): boolean;
begin
  result := q_cmp(op1, op2) >= 0;
end;

// ==== init stuff ====

function alloc_func(alloc_size: sizeuint): pointer; cdecl;
begin
  result := getmem(alloc_size);
end;

procedure free_proc(p: pointer; size: sizeuint); cdecl;
begin
  assert(size = size); // hint off
  freemem(p);
end;

function reallocate_func(p: pointer; old_size, new_size: sizeuint): pointer; cdecl;
begin
  assert(old_size = old_size); // hint off
  result := reallocmem(p, new_size);
end;

var r1: mp_limb_t;

initialization
  // prealloc the GMP's global PRNG state, get rid of the (pseudo) mem leak report
  mpn_random(@r1, 1);
  mp_set_memory_functions(@alloc_func, @reallocate_func, @free_proc);

end.

