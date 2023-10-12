/* The Computer Language Benchmarks Game http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Bert Gijsbers.
 * based on work by Dirk Moerenhout, Jason Alan Palmer and the Go Authors.
 */

package main

import (
    "bufio"
    "bytes"
    "fmt"
    "os"
    "runtime"
    "sort"
    "strings"
)

var toNum = strings.NewReplacer(
    "A", string(0),
    "C", string(1),
    "G", string(3),
    "T", string(2),
)

var toChar = strings.NewReplacer(
    string(0), "A",
    string(1), "C",
    string(3), "G",
    string(2), "T",
)

var empty string

var jobs = []struct {
    length   int
    sequence string
    result   chan string
}{
    {1, empty, make(chan string, 1)},
    {2, empty, make(chan string, 1)},
    {0, "GGT", make(chan string, 1)},
    {0, "GGTA", make(chan string, 1)},
    {0, "GGTATT", make(chan string, 1)},
    {0, "GGTATTTTAATT", make(chan string, 1)},
    {0, "GGTATTTTAATTTATAGT", make(chan string, 1)},
}

func main() {
    dna := input()
    scheduler(dna)
    for i := range jobs {
        fmt.Println(<-jobs[i].result)
    }
}

func scheduler(dna []byte) {
    command := make(chan int, len(jobs))
    for i := runtime.NumCPU(); i > 0; i-- {
        go worker(dna, command)
    }

    for i := range jobs {
        // longest job first, shortest job last
        command <- len(jobs) - 1 - i
    }
    close(command)
}

func worker(dna []byte, command <-chan int) {
    for k := range command {
        if jobs[k].length > 0 {
            jobs[k].result <- frequencyReport(dna, jobs[k].length)
        } else {
            jobs[k].result <- sequenceReport(dna, jobs[k].sequence)
        }
    }
}

func input() (data []byte) {
    data = readThird()
    for i := 0; i < len(data); i++ {
        data[i] = data[i] >> 1 & 3
    }
    return
}

func readThird() (data []byte) {
    in, lineCount := findThree()
    data = make([]byte, 0, lineCount*61)
    for {
        line, err := in.ReadSlice('\n')
        if err != nil && (line == nil || len(line) == 0) || line[0] == '>' {
            break
        }
        last := len(line) - 1
        if line[last] == '\n' {
            line = line[0:last]
        }
        data = append(data, line...)
    }
    return
}

func findThree() (in *bufio.Reader, lineCount int) {
    in = bufio.NewReaderSize(os.Stdin, 1<<20)
    for {
        line, err := in.ReadSlice('\n')
        if err != nil {
            panic("read error")
        }
        lineCount++
        if line[0] == '>' && strings.HasPrefix(string(line), ">THREE") {
            break
        }
    }
    return
}

type sequence struct {
    nucs  string
    count uint32
}

type sequenceSlice []sequence

func (ss sequenceSlice) Len() int {
    return len(ss)
}

func (ss sequenceSlice) Swap(i, j int) {
    ss[i], ss[j] = ss[j], ss[i]
}

func (ss sequenceSlice) Less(i, j int) bool {
    if ss[i].count == ss[j].count {
        return ss[i].nucs > ss[j].nucs
    }
    return ss[i].count > ss[j].count
}

func frequencyReport(dna []byte, length int) string {
    var sortedSeqs sequenceSlice
    counts := count32(dna, length)
    for num, pointer := range counts {
        sortedSeqs = append(
            sortedSeqs,
            sequence{toChar.Replace(string(decompress32(num, length))), *pointer},
        )
    }
    sort.Sort(sortedSeqs)

    var buf bytes.Buffer
    for _, sequence := range sortedSeqs {
        buf.WriteString(fmt.Sprintf(
            "%v %.3f\n", sequence.nucs,
            100.0*float32(sequence.count)/float32(len(dna)-length+1)),
        )
    }
    return buf.String()
}

func sequenceReport(dna []byte, sequence string) string {
    var pointer *uint32
    if len(sequence) <= 16 {
        counts := count32(dna, len(sequence))
        pointer = counts[compress32([]byte(toNum.Replace(sequence)))]
    } else {
        counts := count64(dna, len(sequence))
        pointer = counts[compress64([]byte(toNum.Replace(sequence)))]
    }
    var sequenceCount uint32
    if pointer != nil {
        sequenceCount = *pointer
    }
    return fmt.Sprintf("%v\t%v", sequenceCount, sequence)
}

func count32(dna []byte, length int) map[uint32]*uint32 {
    counts := make(map[uint32]*uint32)
    key := compress32(dna[0 : length-1])
    mask := uint32(1)<<uint(2*length) - 1
    for index := length - 1; index < len(dna); index++ {
        key = key<<2&mask | uint32(dna[index])
        pointer, ok := counts[key]
        if !ok {
            pointer = new(uint32)
            counts[key] = pointer
        }
        *pointer++
    }
    return counts
}

func count64(dna []byte, length int) map[uint64]*uint32 {
    counts := make(map[uint64]*uint32)
    key := compress64(dna[0 : length-1])
    mask := uint64(1)<<uint(2*length) - 1
    for index := length - 1; index < len(dna); index++ {
        key = key<<2&mask | uint64(dna[index])
        pointer, ok := counts[key]
        if !ok {
            pointer = new(uint32)
            counts[key] = pointer
        }
        *pointer++
    }
    return counts
}

func compress64(sequence []byte) uint64 {
    var num uint64
    for _, char := range sequence {
        num = num<<2 | uint64(char)
    }
    return num
}

func compress32(sequence []byte) uint32 {
    var num uint32
    for _, char := range sequence {
        num = num<<2 | uint32(char)
    }
    return num
}

func decompress32(num uint32, length int) (sequence []byte) {
    sequence = make([]byte, length)
    for i := 0; i < length; i++ {
        sequence[length-i-1] = byte(num & 3)
        num = num >> 2
    }
    return
}