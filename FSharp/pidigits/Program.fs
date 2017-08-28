
module PiDigits
open System
open System.Runtime.InteropServices

[<StructLayout (LayoutKind.Sequential)>]
[<Struct>]
type mpz_t =
   val _mp_alloc:int
   val _mp_size:int
   val ptr:IntPtr

[<DllImport ("gmp", EntryPoint="__gmpz_init")>]
let mpz_init(value : mpz_t byref) : unit = failwith ""

[<DllImport ("gmp", EntryPoint="__gmpz_mul_si")>]
let mpz_mul_si(dest : mpz_t byref, src : mpz_t byref, value : int) : unit = failwith ""

[<DllImport ("gmp", EntryPoint="__gmpz_add")>]
let mpz_add(dest : mpz_t byref, src : mpz_t byref, src2 : mpz_t byref) : unit = failwith ""

[<DllImport ("gmp", EntryPoint="__gmpz_tdiv_q")>]
let mpz_tdiv_q(dest : mpz_t byref, src : mpz_t byref, src2 : mpz_t byref) : unit = failwith ""

[<DllImport ("gmp", EntryPoint="__gmpz_set_si")>]
let mpz_set_si(src : mpz_t byref, value : int) : unit = failwith ""

[<DllImport ("gmp", EntryPoint="__gmpz_get_si")>] 
let mpz_get_si(src : mpz_t byref) : int = 0

let init() = 
    let mutable result = mpz_t()
    mpz_init(&result)
    result

let mutable q,r,s,t,u,v,w = init(),init(),init(),init(),init(),init(),init()

let mutable i = 0
let mutable c = 0
let ch = Array.zeroCreate 10
let args = System.Environment.GetCommandLineArgs()
let n = Int32.Parse args.[1]
let intZero = int '0'

let inline compose_r(bq, br, bs, bt) = 
    mpz_mul_si(&u, &r, bs)
    mpz_mul_si(&r, &r, bq)
    mpz_mul_si(&v, &t, br)
    mpz_add(&r, &r, &v)
    mpz_mul_si(&t, &t, bt)
    mpz_add(&t, &t, &u)
    mpz_mul_si(&s, &s, bt)
    mpz_mul_si(&u, &q, bs)
    mpz_add(&s, &s, &u)
    mpz_mul_si(&q, &q, bq)

// Compose matrix with numbers on the left.
let inline compose_l(bq, br, bs, bt) =
    mpz_mul_si(&r, &r, bt)
    mpz_mul_si(&u, &q, br)
    mpz_add(&r, &r, &u)
    mpz_mul_si(&u, &t, bs)
    mpz_mul_si(&t, &t, bt)
    mpz_mul_si(&v, &s, br)
    mpz_add(&t, &t, &v)
    mpz_mul_si(&s, &s, bq)
    mpz_add(&s, &s, &u)
    mpz_mul_si(&q, &q, bq)

// Extract one digit.
let inline extract(j) = 
    mpz_mul_si(&u, &q, j)
    mpz_add(&u, &u, &r)
    mpz_mul_si(&v, &s, j)
    mpz_add(&v, &v, &t)
    mpz_tdiv_q(&w, &u, &v)
    mpz_get_si(&w)


// Print one digit. Returns 1 for the last digit. 
let inline prdigit(y:int) = 
    ch.[c] <- char (intZero + y)
    c <- c + 1
    i <- i + 1
    if (i%10=0 || i = n) then
        while c<>ch.Length do
            ch.[c] <- ' '
            c<-c+1
        c <- 0
        Console.Write(ch)
        Console.Write("\t:")
        Console.WriteLine(i)
    i = n

// Generate successive digits of PI. 
let mutable k = 1
i <- 0
mpz_set_si(&q, 1)
mpz_set_si(&r, 0)
mpz_set_si(&s, 0)
mpz_set_si(&t, 1)
let mutable more = true
while more do
    let y = extract 3
    if y = extract 4 then
        if prdigit y then more<-false
        else compose_r(10, -10*y, 0, 1)
    else
        compose_l(k, 4*k+2, 0, 2*k+1);
        k<-k+1
