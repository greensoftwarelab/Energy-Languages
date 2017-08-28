// The Computer Language Benchmark Game
// http://benchmarksgame.alioth.debian.org/
//
// Inspired by the gcc implementation
// contributed by Ralph Ganszky

import gmp

// Get command line options
let n: UInt
if CommandLine.argc > 1 {
    n = UInt(CommandLine.arguments[1]) ?? 50
} else {
    n = 50
}

// Allocate numbers and defer their destruction
let tmp1 = UnsafeMutablePointer<mpz_t>.allocate(capacity: 1)
let tmp2 = UnsafeMutablePointer<mpz_t>.allocate(capacity: 1)
let acc = UnsafeMutablePointer<mpz_t>.allocate(capacity: 1)
let den = UnsafeMutablePointer<mpz_t>.allocate(capacity: 1)
let num = UnsafeMutablePointer<mpz_t>.allocate(capacity: 1)
defer {
    __gmpz_clear(tmp1)
    __gmpz_clear(tmp2)
    __gmpz_clear(acc)
    __gmpz_clear(den)
    __gmpz_clear(num)

    tmp1.deallocate(capacity: 1)
    tmp2.deallocate(capacity: 1)
    acc.deallocate(capacity: 1)
    den.deallocate(capacity: 1)
    num.deallocate(capacity: 1)
}

// Initialize numbers
__gmpz_init(tmp1)
__gmpz_init(tmp2)

__gmpz_init_set_ui(acc, 0)
__gmpz_init_set_ui(den, 1)
__gmpz_init_set_ui(num, 1)

func extractDigit(_ n: UInt) -> UInt {
    __gmpz_mul_ui(tmp1, num, n)
    __gmpz_add(tmp2, tmp1, acc)
    __gmpz_tdiv_q(tmp1, tmp2, den)
    return __gmpz_get_ui(tmp1)
}

func eliminateDigit(_ d: UInt) {
    __gmpz_submul_ui(acc, den, d)
    __gmpz_mul_ui(acc, acc, 10)
    __gmpz_mul_ui(num, num, 10)
}

func nextTerm(_ k: UInt) {
    let k2 = 2*k+1
    __gmpz_addmul_ui(acc, num, 2)
    __gmpz_mul_ui(acc, acc, k2)
    __gmpz_mul_ui(den, den, k2)
    __gmpz_mul_ui(num, num, k)
}

var d: UInt
var d4: UInt
var k: UInt = 0
var i: UInt = 0

// Generate digits
for i in 1...n {
    repeat {
	repeat {
	    k += 1
	    nextTerm(k)
	} while __gmpz_cmp(num, acc) > 0

	d = extractDigit(3)
	d4 = extractDigit(4)
    } while d != d4

    // Output digit
    print("\(d)", terminator: "")
    if i % 10 == 0 {
	print("\t:\(i)")
    }

    eliminateDigit(d)
}
if n % 10 != 0 {
    for _ in 0..<(10-(n%10)) {
	print(" ", terminator: "")
    }
    print("\t:\(i)")
}
