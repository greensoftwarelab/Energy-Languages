/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Dirk Moerenhout.
 */

package main

import (
   "bufio"
   "os"
   "runtime"
)

const bufLines = 1024 * 16
const lineSize = 60

var complement = [256]uint8{
   'A': 'T', 'a': 'T',
   'C': 'G', 'c': 'G',
   'G': 'C', 'g': 'C',
   'T': 'A', 't': 'A',
   'U': 'A', 'u': 'A',
   'M': 'K', 'm': 'K',
   'R': 'Y', 'r': 'Y',
   'W': 'W', 'w': 'W',
   'S': 'S', 's': 'S',
   'Y': 'R', 'y': 'R',
   'K': 'M', 'k': 'M',
   'V': 'B', 'v': 'B',
   'H': 'D', 'h': 'D',
   'D': 'H', 'd': 'H',
   'B': 'V', 'b': 'V',
   'N': 'N', 'n': 'N',
}

func createOutThread(inc chan [][]byte, statec chan int) {
   work := <-inc
   for {
      inbuf := work[0]
      outbuf := work[1]
      lines := len(inbuf) / lineSize
      linelength := lineSize
      if lines == 0 {
         linelength = len(inbuf)
      }
      for obufpos, bufpos := 0, len(inbuf)-1; bufpos >= 0; {
         for end := obufpos + linelength; obufpos < end; obufpos++ {
            outbuf[obufpos] = complement[inbuf[bufpos]]
            bufpos--
         }
         outbuf[obufpos] = '\n'
         obufpos++
      }
      statec <- 1
      work = <-inc
   }
}

func doOutThread(inc chan []byte, statec chan int) {
   work := <-inc
   for {
      os.Stdout.Write(work)
      statec <- 1
      work = <-inc
   }
}

func main() {
   ncpu := runtime.NumCPU()
   runtime.GOMAXPROCS(ncpu)

   in := bufio.NewReader(os.Stdin)
   buf := make([]byte, 0, lineSize*2*1024*1024)

   prepstatec := make(chan int, ncpu)
   var cpuic []chan [][]byte = make([]chan [][]byte, ncpu)
   for i := 0; i < ncpu; i++ {
      cpuic[i] = make(chan [][]byte, 1)
      go createOutThread(cpuic[i], prepstatec)
   }

   outc := make(chan []byte, 1)
   outstatec := make(chan int, 1)
   outstatec <- 1
   go doOutThread(outc, outstatec)

   var obuf [2][]byte
   obuf[0] = make([]byte, (lineSize+1)*bufLines*ncpu*2)
   obuf[1] = obuf[0][(lineSize+1)*bufLines*ncpu:]
   obuf[0] = obuf[0][:(lineSize+1)*bufLines*ncpu]
   activeobuf := 0

   line, err := in.ReadSlice('\n')
   for err == nil {
      title := make([]byte, len(line))
      copy(title, line)

   BulkRead:
      for {
         for lines := cap(buf)/lineSize - len(buf)/lineSize; lines > 0; lines-- {
            line, err = in.Peek(61)
            if err != nil {
               line, err = in.ReadSlice('\n')
               if err != nil || line[0] == '>' {
                  break BulkRead
               }
               buf = append(buf, line[0:len(line)-1]...)
               continue
            }
            if line[0] == '>' {
               line, err = in.ReadSlice('\n')
               break BulkRead
            }
            if line[lineSize] == '\n' {
               buf = append(buf, line[0:lineSize]...)
               in.Discard(lineSize + 1)
               continue
            }
            line, err = in.ReadSlice('\n')
            buf = append(buf, line[0:len(line)-1]...)
         }
         nbuf := make([]byte, len(buf), cap(buf)+lineSize*1024*1024)
         copy(nbuf, buf)
         buf = nbuf
      }

      <-outstatec
      outc <- title

      charsleft := len(buf) % lineSize

      for bufend, lines := len(buf), len(buf)/lineSize; lines > 0; {
         linesperthread := bufLines
         linesptmod := 0
         if lines < bufLines*ncpu {
            linesperthread = lines / ncpu
            linesptmod = lines % ncpu
         }
         obufstart := 0
         obufend := 0
         for i := 0; i < ncpu; i++ {
            if i == (ncpu - 1) {
               linesperthread += linesptmod
            }
            lines -= linesperthread
            bufstart := bufend - linesperthread*lineSize
            obufend = obufstart + linesperthread*(lineSize+1)
            prepworkorder := [][]byte{buf[bufstart:bufend], obuf[activeobuf][obufstart:obufend]}
            cpuic[i] <- prepworkorder
            bufend = bufstart
            obufstart = obufend
         }
         for i := 0; i < ncpu; i++ {
            <-prepstatec
         }
         <-outstatec
         outc <- obuf[activeobuf][0:obufend]
         activeobuf = activeobuf ^ 1
      }
      if charsleft > 0 {
         prepworkorder := [][]byte{buf[0:charsleft], obuf[activeobuf][0 : charsleft+1]}
         cpuic[0] <- prepworkorder
         <-prepstatec
         <-outstatec
         outc <- obuf[activeobuf][0 : charsleft+1]
      }
      buf = buf[0:0]
   }
   <-outstatec
}