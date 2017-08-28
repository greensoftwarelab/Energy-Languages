// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// regex-dna program contributed by Valentin Kraevskiy
// converted from regex-dna program

open System.Text.RegularExpressions

let regex s = Regex (s, RegexOptions.Compiled)
let input = stdin.ReadToEnd ()
let text = (regex ">.*\n|\n").Replace (input, "")

["agggtaaa|tttaccct"
 "[cgt]gggtaaa|tttaccc[acg]"
 "a[act]ggtaaa|tttacc[agt]t"
 "ag[act]gtaaa|tttac[agt]ct"
 "agg[act]taaa|ttta[agt]cct"
 "aggg[acg]aaa|ttt[cgt]ccct"
 "agggt[cgt]aa|tt[acg]accct"
 "agggta[cgt]a|t[acg]taccct"
 "agggtaa[cgt]|[acg]ttaccct"]
|> List.iter (fun s ->
         printf "%s %i\n" s ((regex s).Matches text).Count)

let newText =
    ["tHa[Nt]", "<4>"
     "aND|caN|Ha[DS]|WaS", "<3>"
     "a[NSt]|BY", "<2>"
     "<[^>]*>", "|"
     "\\|[^|][^|]*\\|" , "-"]
     |> List.fold (fun s (code, alt) -> 
            (regex code).Replace (s, alt)) text

printf "\n%i\n%i\n%i\n" input.Length text.Length newText.Length