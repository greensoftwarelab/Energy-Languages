/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   direct transliteration of Greg Buchholz's C program
   contributed by Isaac Gouy
*/

/// <reference path="../node_modules/@types/node/index.d.ts" />

const w = +process.argv[2]
const h = w

let bit_num = 0, i = 0, byte_acc = 0
const iter = 50, limit = 2.0
let Zr, Zi, Cr, Ci, Tr, Ti

process.stdout.write( "P4\n" + w + " " + h + "\n" )

for (let y = 0; y < h; ++y) {
   for (let x = 0; x < w; ++x) {

      Zr = 0.0; Zi = 0.0; Tr = 0.0; Ti = 0.0
      Cr = 2.0*x/w - 1.5; Ci = 2.0*y/h - 1.0

      for (let i = 0; i < iter && (Tr+Ti <= limit*limit); ++i) {
         Zi = 2.0*Zr*Zi + Ci
         Zr = Tr - Ti + Cr
         Tr = Zr * Zr
         Ti = Zi * Zi
      }

      byte_acc <<= 1
      if (Tr+Ti <= limit*limit) { byte_acc |= 0x01 }

      ++bit_num

      if (bit_num == 8) {
         process.stdout.write( String.fromCharCode(byte_acc),'ascii' )
         byte_acc = 0
         bit_num = 0
      }
      else if (x == w-1) {
         byte_acc <<= (8-w%8)
         process.stdout.write( String.fromCharCode(byte_acc),'ascii' )
         byte_acc = 0
         bit_num = 0
      }
   }
}
