/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Martin Koistinen
 * Based on mandelbrot.c contributed by Greg Buchholz and The Go Authors
 * flag.Arg hack by Isaac Gouy
 *
 * Large changes by Bill Broadley, including:
 * 1) Switching the one goroutine per line to one per CPU
 * 2) Replacing gorouting calls with channels
 * 3) Handling out of order results in the file writer.

 * modified by Sean Lake
 *
 * modified by Rodrigo Corsi Oct 04/2014
 * 1)two goroutines per cpu
 * 2)each goroutine generate one line and increment counter (atomic int32)
 */

package main

import (
   "bufio"
   "flag"
   "fmt"
   "os"
   "runtime"
   "strconv"
   "sync"
   "sync/atomic"
)

const LIMIT = 4.0 //2*2
const ITER = 50 // Benchmark parameter

var pool int
var yAt int32 = -1
var fields [][]byte
var size = 4000
var bytesPerRow int
var initial_r []float64
var initial_i []float64

// This func is responsible for rendering a row of pixels
func renderRow(y0 *int32) []byte{
   var i, j, x, xb int
   var res, b byte
   var Zr1, Zr2, Zi1, Zi2, Tr1, Tr2, Ti1, Ti2 float64

   field := make([]byte, bytesPerRow)

   for x=0; x<bytesPerRow; x++{
      res=0
      Ci := initial_i[*y0]
      for i=0; i<8; i+=2{
         xb = x<<3
         Cr1:=initial_r[xb+i]
         Cr2:=initial_r[xb+i+1]

         Zr1=Cr1
         Zi1=Ci

         Zr2=Cr2
         Zi2=Ci

         b = 0

         for j=0; j<ITER; j++{
            Tr1 = Zr1*Zr1
            Ti1 = Zi1*Zi1
            Zi1 = 2*Zr1*Zi1+Ci
            Zr1 = Tr1-Ti1+Cr1 

            if(Tr1+Ti1>LIMIT){
               b|=2
               if(b==3){
                  break
               }
            }

            Tr2 = Zr2*Zr2
            Ti2 = Zi2*Zi2
            Zi2 = 2*Zr2*Zi2+Ci
            Zr2 = Tr2-Ti2+Cr2 

            if(Tr2+Ti2>LIMIT){
               b|=1
               if(b==3){
                  break
               }
            }
         }
         res= (res<<2) | b
      }
      field[x] = ^res 
   }
   return field
}

func renderRows(wg *sync.WaitGroup, s32 int32){
   var y int32
   for y=atomic.AddInt32(&yAt, 1); y<s32; y=atomic.AddInt32(&yAt, 1){
      fields[y] = renderRow( &y )
   }
   wg.Done()
}

func main() {
   pool = runtime.NumCPU() << 1
   runtime.GOMAXPROCS(pool)
   
   // Get input, if any...
   flag.Parse()
   if flag.NArg() > 0 {
      size, _ = strconv.Atoi(flag.Arg(0))
   }

   bytesPerRow = size >> 3

   // Precompute the initial real and imaginary values for each x and y
   // coordinate in the image.
   initial_r = make([]float64, size)
   initial_i = make([]float64, size)
   inv := 2.0 / float64(size)
   for xy:=0; xy<size; xy++{
      i:=inv*float64(xy)
      initial_r[xy]=i - 1.5
      initial_i[xy]=i - 1.0
   }

   fields = make([][]byte, size)

   /* Wait group for finish */
   wg := new(sync.WaitGroup)
   wg.Add(pool)
      
   var s32 int32 = int32(size)//to compare with y int32
   // start pool workers, and assign all work
   for i := 0; i < pool; i++ {
      go renderRows(wg, s32)
   }

   /* wait for the file workers to finish, then write */
   wg.Wait()

   out := bufio.NewWriter(os.Stdout)
   defer out.Flush()
   fmt.Fprintf(out, "P4\n%d %d\n", size, size)

   for y:=0 ; y<size ; y++{
      out.Write(fields[y])
   }
}