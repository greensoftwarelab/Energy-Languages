/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Isaac Gouy
*/
"use strict";
/// <reference path="../node_modules/@types/node/index.d.ts" />
function fannkuch(n) {
    const perm = new Array(n), count = new Array(n);
    const perm1 = new Array(n);
    for (let i = 0; i < n; i++) {
        perm1[i] = i;
    }
    let f = 0, i = 0, k = 0, r = 0, flips = 0, nperm = 0, checksum = 0;
    r = n;
    while (r > 0) {
        i = 0;
        while (r != 1) {
            count[r - 1] = r;
            r -= 1;
        }
        while (i < n) {
            perm[i] = perm1[i];
            i += 1;
        }
        // Count flips and update max  and checksum
        f = 0;
        k = perm[0];
        while (k != 0) {
            i = 0;
            while (2 * i < k) {
                const t = perm[i];
                perm[i] = perm[k - i];
                perm[k - i] = t;
                i += 1;
            }
            k = perm[0];
            f += 1;
        }
        if (f > flips) {
            flips = f;
        }
        if ((nperm & 0x1) == 0) {
            checksum += f;
        }
        else {
            checksum -= f;
        }
        // Use incremental change to generate another permutation
        let go = true;
        while (go) {
            if (r == n) {
                console.log(checksum);
                return flips;
            }
            let p0 = perm1[0];
            i = 0;
            while (i < r) {
                const j = i + 1;
                perm1[i] = perm1[j];
                i = j;
            }
            perm1[r] = p0;
            count[r] -= 1;
            if (count[r] > 0) {
                go = false;
            }
            else {
                r += 1;
            }
        }
        nperm += 1;
    }
    return flips;
}
const n = +process.argv[2];
console.log("Pfannkuchen(" + n + ") = " + fannkuch(n));
