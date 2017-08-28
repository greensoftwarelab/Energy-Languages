

#nowarn "9"

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Threading
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

let toBytes (s: string) = s.ToCharArray() |> Array.map byte
let toString (s: byte []) = String(s |> Array.map char)
let prefixes = [| "ggt"; "ggta"; "ggtatt"; "ggtattttaatt"; "ggtattttaatttatagt" |]

let prefixBytes = 
    [| for p in prefixes -> toBytes p |]

let prefixLengths = 
    [| for p in prefixBytes -> p.Length |]

let prefixOffsets = Array.scan (+) 0 prefixLengths
let dnaStart = prefixOffsets.[prefixLengths.Length]

let createDNA() = 
    //let input = File.OpenText("knucleotide-input-2500000.txt")
    let input = new StreamReader(Console.OpenStandardInput())
    
    let text = 
        seq { 
            while true do
                yield input.ReadLine()
        }
        |> Seq.takeWhile (fun x -> x <> null)
        |> Seq.skipWhile (fun x -> not (x.StartsWith(">THREE")))
        |> Seq.skip 1
        |> String.concat ""
    
    // convert the text to a pinned array of bytes
    let bytes = 
        text
        |> toBytes
        |> Array.append (Array.concat prefixBytes)
    
    let h = GCHandle.Alloc(bytes, GCHandleType.Pinned)
    let addr = h.AddrOfPinnedObject() |> NativePtr.ofNativeInt
    addr, bytes.Length

let dna, dnaLength = createDNA()
let inline readDNA i = NativePtr.get dna i

let inline readDNABytes s n = 
    let res = Array.zeroCreate n
    for i in 0..n - 1 do
        res.[i] <- NativePtr.get dna (s + i)
    res

let keyHash (k, n) = 
    let mutable hash = 0
    for i in 0..n - 1 do
        hash <- hash * 31 + int (readDNA (k + i))
    hash

let keyText (k, n) = toString(readDNABytes k n).ToUpper()

type Value = 
    { mutable value: int
      key: int * int }

let generateFrequencies (frameSize) = 
    let freq = Dictionary<int, Value>()
    let mutable v = Unchecked.defaultof<Value>
    for i in dnaStart..dnaLength - frameSize do
        let h = keyHash (i, frameSize)
        if freq.TryGetValue(h, &v) then v.value <- v.value + 1
        else freq.Add(h, { value = 1; key = (i, frameSize) })
    freq

let writeCount (n: int) = 
    let freq = generateFrequencies (prefixLengths.[n])
    let hash = keyHash (prefixOffsets.[n], prefixLengths.[n])
    
    let count = 
        if freq.ContainsKey(hash) then freq.[hash].value
        else 0
    String.Format("{0}\t{1}", count, prefixes.[n].ToUpper())

type Pair = KeyValuePair<int, Value>

let writeFrequencies (nucleotideLength) = 
    let freq = generateFrequencies (nucleotideLength)
    let items = new ResizeArray<Pair>(freq)
    items.Sort(fun (p1: Pair) (p2: Pair) -> 
        let c = p2.Value.value - p1.Value.value
        if c = 0 then keyText(p1.Value.key).CompareTo(keyText(p2.Value.key))
        else c)
    let buf = new StringBuilder()
    let sum = dnaLength - dnaStart - nucleotideLength + 1
    for element in items do
        let percent = double element.Value.value * 100.0 / double sum
        buf.AppendFormat("{0} {1:f3}\n", keyText element.Value.key, percent) |> ignore
    buf.ToString()

let runTasks (tasks: (unit -> 'T) []) = 
    let taskCount = ref tasks.Length
    let results = Array.zeroCreate tasks.Length
    
    let rec worker() = 
        let j = Interlocked.Decrement(&taskCount.contents)
        if j >= 0 then 
            results.[j] <- tasks.[j]()
            worker()
    
    let threads = 
        Array.init Environment.ProcessorCount (fun i -> 
            let t = new Thread(worker)
            t.Start()
            t)
    
    for t in threads do
        t.Join()
    results

let results = 
    runTasks [| yield (fun () -> writeFrequencies 1)
                yield (fun () -> writeFrequencies 2)
                for i in 0..prefixes.Length - 1 do
                    yield (fun () -> writeCount i) |]

//let endTime = System.DateTime.Now.Ticks
for s in results do
    Console.Out.WriteLine(s)
