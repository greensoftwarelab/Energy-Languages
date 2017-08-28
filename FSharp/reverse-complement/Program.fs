// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by Jimmy Tang

open System;
open System.IO
open System.Text.RegularExpressions;

let out = Console.OpenStandardOutput()

let bytemap =
    let lookup = Array.zeroCreate 128
    for a,b in Seq.zip "ACBDGHKMNSRUTWVYacbdghkmnsrutwvy"B "TGVHCDMKNSYAAWBRTGVHCDMKNSYAAWBR"B do lookup.[int a] <- b
    lookup

let revLine (s:string) shift =
    let bytes = Text.Encoding.ASCII.GetBytes(s)
    let L = Array.length bytes
    for i in 0..(L - 1) do
        bytes.[i] <- bytemap.[int bytes.[i]]
    for i in 0..(L / 2 - 1) do
        let t = bytes.[i]
        bytes.[i] <- bytes.[L - 1 - i]
        bytes.[L - 1 - i] <- t
    out.Write(bytes, 0, L-shift)
    if shift <> 0 then
        out.WriteByte('\n'B)
        out.Write(bytes, L-shift, shift)

let revcomp title lines =
    printfn "%s" title
    match lines with
    | first::rest ->
        revLine first 0
        let rec loop = function
            | [] -> out.WriteByte('\n'B)
            | (line:string) :: rest ->
                revLine line (String.length first)
                loop rest
        loop rest
    | _ -> ()

let rec run title data =
    match Console.In.ReadLine() with
        | null -> revcomp title data
        | line when line.[0] = '>' ->
            if title <> "" then
                revcomp title data
            run line []
        | line ->
            run title (line::data)
run "" []