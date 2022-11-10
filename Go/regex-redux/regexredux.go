/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * regex-dna program contributed by The Go Authors.
 * modified by Tylor Arndt.
 * modified by Chandra Sekar S to use optimized PCRE binding.
 * converted from regex-dna program
 */

package main

import (
   "fmt"
   "io/ioutil"
   "os"
   "runtime"

   "github.com/glenn-brown/golang-pkg-pcre/src/pkg/pcre"
)
//   "github.com/tuxychandru/golang-pkg-pcre/src/pkg/pcre"


var variants = []string{
   "agggtaaa|tttaccct",
   "[cgt]gggtaaa|tttaccc[acg]",
   "a[act]ggtaaa|tttacc[agt]t",
   "ag[act]gtaaa|tttac[agt]ct",
   "agg[act]taaa|ttta[agt]cct",
   "aggg[acg]aaa|ttt[cgt]ccct",
   "agggt[cgt]aa|tt[acg]accct",
   "agggta[cgt]a|t[acg]taccct",
   "agggtaa[cgt]|[acg]ttaccct",
}

type Subst struct {
   pat, repl string
}

var substs = []Subst{
   {"tHa[Nt]", "<4>"},
   {"aND|caN|Ha[DS]|WaS", "<3>"},
   {"a[NSt]|BY", "<2>"},
   {"<[^>]*>", "|"},
   {"\\|[^|][^|]*\\|", "-"},
}

func countMatches(pat string, bytes []byte) int {
   m := pcre.MustCompile(pat, 0).Matcher(bytes, 0)
   n := 0
   for f := m.Matches(); f; f = m.Match(bytes, 0) {
      n++
      bytes = bytes[m.Index()[1]:]
   }
   return n
}

func main() {
   runtime.GOMAXPROCS(runtime.NumCPU())
   bytes, err := ioutil.ReadAll(os.Stdin)
   if err != nil {
      fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
      os.Exit(2)
   }
   ilen := len(bytes)
   // Delete the comment lines and newlines
   bytes = pcre.MustCompile("(>[^\n]+)?\n", 0).ReplaceAll(bytes, []byte{}, 0)
   clen := len(bytes)

   mresults := make([]chan int, len(variants))
   var i int
   var s string
   for i, s = range variants {
      ch := make(chan int)
      mresults[i] = ch
      go func(intch chan int, ss string) {
         intch <- countMatches(ss, bytes)
      }(ch, s)
   }

   lenresult := make(chan int)
   bb := bytes
   go func() {
      for _, sub := range substs {
         bb = pcre.MustCompile(sub.pat, 0).ReplaceAll(bb, []byte(sub.repl), 0)
      }
      lenresult <- len(bb)
   }()

   for i, s = range variants {
      fmt.Printf("%s %d\n", s, <-mresults[i])
   }
   fmt.Printf("\n%d\n%d\n%d\n", ilen, clen, <-lenresult)
}
