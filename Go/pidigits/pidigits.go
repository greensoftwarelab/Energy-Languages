/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * based on pidigits.c (by Paolo Bonzini & Sean Bartlett,
 *    modified by Michael Mellor)
 *
 * contributed by Zhao Zhiqiang.
 */

package main

/*
#cgo LDFLAGS: -lgmp
#include <gmp.h>
#include <stdlib.h>
*/
import "C"

import (
    "bufio"
    "flag"
    "fmt"
    "os"
    "runtime"
    "strconv"
)

var n = 0

func extract_digit(nth uint32) uint32 {
    C.mpz_mul_ui(&tmp1[0], &num[0], C.ulong(nth))
    C.mpz_add   (&tmp2[0], &tmp1[0], &acc[0])
    C.mpz_tdiv_q(&tmp1[0], &tmp2[0], &den[0])

    return uint32(C.mpz_get_ui(&tmp1[0]))
}

func eliminate_digit(d uint32) {
    C.mpz_submul_ui(&acc[0], &den[0], C.ulong(d))
    C.mpz_mul_ui(&acc[0], &acc[0], 10)
    C.mpz_mul_ui(&num[0], &num[0], 10)    
}

func next_term(k uint32) {
    k2 := C.ulong(k*2+1)

    C.mpz_addmul_ui(&acc[0], &num[0], 2)
    C.mpz_mul_ui(&acc[0], &acc[0], k2)
    C.mpz_mul_ui(&den[0], &den[0], k2)
    C.mpz_mul_ui(&num[0], &num[0], C.ulong(k))        
}

func init() {
    runtime.GOMAXPROCS(1)
    flag.Parse()
    if flag.NArg() > 0 {
        n, _ = strconv.Atoi(flag.Arg(0))
    }
}

var tmp1, tmp2, acc, den, num C.mpz_t

func main() {
    w := bufio.NewWriter(os.Stdout)
    defer w.Flush()

    C.mpz_init(&tmp1[0])
    C.mpz_init(&tmp2[0])

    C.mpz_init_set_ui(&acc[0], 0)
    C.mpz_init_set_ui(&den[0], 1)
    C.mpz_init_set_ui(&num[0], 1)

    k := uint32(0)
    d := uint32(0)
    for i := 0; i < n; {
        k++
        next_term(k)

        if C.mpz_cmp(&num[0], &acc[0]) > 0 {
            continue
        }

        d = extract_digit(3)
        if d != extract_digit(4) {
            continue
        }

        fmt.Printf("%d", d)

        i++
        if i % 10 == 0 {
            fmt.Printf("\t:%d\n", i)
        }

        eliminate_digit(d)
    } 
}