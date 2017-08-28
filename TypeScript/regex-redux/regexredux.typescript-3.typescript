/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   by Josh Goldfoot, adapted from the node.js version
   compile with tsc --lib es7 regexredux.ts
*/

/// <reference path="../node_modules/@types/node/index.d.ts" />

var fs = require("fs");

var i = fs.readFileSync("/dev/stdin", "ascii");
var ilen = i.length;
i = i.replace(/^>.*\n|\n/mg, "");
var clen = i.length;

var iubReplaceLen: Promise<number> = new Promise<number>(resolve => {
    var iub: string[] = ["-", "|", "<2>", "<3>", "<4>"];
    var iubR: RegExp[] = [/\|[^|][^|]*\|/g, /<[^>]*>/g, /a[NSt]|BY/g, 
        /aND|caN|Ha[DS]|WaS/g, /tHa[Nt]/g];
    var seq = i + "";
    while (iub.length)
        seq = seq.replace(iubR.pop(), iub.pop())
    resolve(seq.length);
});
    
var q: RegExp[] = [/agggtaaa|tttaccct/ig, /[cgt]gggtaaa|tttaccc[acg]/ig,
    /a[act]ggtaaa|tttacc[agt]t/ig, /ag[act]gtaaa|tttac[agt]ct/ig,
    /agg[act]taaa|ttta[agt]cct/ig, /aggg[acg]aaa|ttt[cgt]ccct/ig,
    /agggt[cgt]aa|tt[acg]accct/ig, /agggta[cgt]a|t[acg]taccct/ig,
    /agggtaa[cgt]|[acg]ttaccct/ig];

var promises: Promise<string>[] = q.map(r => new Promise<string>(resolve => {
    var m: RegExpMatchArray = i.match(r);
    resolve(r.source + " " + (m ? m.length : 0));
}));

async function displayOutput(promises: Promise<string>[], ilen: number,
        clen: number, iubReplaceLen: Promise<number>): Promise<void> {
    for (var count = 0; count < 9; count++)
        console.log(await promises[count]);

    console.log();
    console.log(ilen);
    console.log(clen);
    console.log(await iubReplaceLen);
}

displayOutput(promises, ilen, clen, iubReplaceLen);
    
