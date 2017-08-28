// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Modification by Don Syme & Jomo Fisher to use null as representation
// of Empty node and to use a single Next element.
// Based on F# version by Robert Pickering
// Based on ocaml version by Troestler Christophe & Isaac Gouy
// *reset*

open System
open Unchecked

type Next = { Left: Tree; Right: Tree }
and [<Struct>] Tree(next:Next) =
    member t.Check() =
        match box next with 
        | null -> 1
        | _ -> 1 + next.Left.Check() + next.Right.Check()

let rec make depth =
    if depth > 0 then Tree({Left = make (depth-1); Right=make (depth-1)})
    else Tree(defaultof<_>)

let inline check (tree:Tree) = tree.Check()

let rec loopDepths maxDepth minDepth d =
    if d <= maxDepth then
        let niter = 1 <<< (maxDepth - d + minDepth)
        let mutable c = 0
        for i = 1 to niter do
            c <- c + check (make d)
        Console.WriteLine("{0}\t trees of depth {1}\t check: {2}",niter,d,c)
        loopDepths maxDepth minDepth (d + 2)

[<EntryPoint>]
let main args =
    let minDepth = 4
    let maxDepth =
        let n = if args.Length > 0 then int args.[0] else 10
        max (minDepth + 2) n
    let stretchDepth = maxDepth + 1

    let c = check (make stretchDepth)
    Console.WriteLine("stretch tree of depth {0}\t check: {1}",stretchDepth,c)
    let longLivedTree = make maxDepth
    loopDepths maxDepth minDepth minDepth
    Console.WriteLine("long lived tree of depth {0}\t check: {1}",maxDepth,(check longLivedTree))
    exit 0