/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Josh Goldfoot
*/
/// <reference path="../node_modules/@types/node/index.d.ts" />
import { createInterface } from "readline";
class RefNum {
    constructor(n) {
        this.num = n;
    }
}
function frequency(sequence, length) {
    var freq = new Map();
    var n = sequence.length - length + 1;
    var sub = "";
    var m;
    for (var i = 0; i < n; i++) {
        sub = sequence.substr(i, length);
        m = freq.get(sub);
        if (m === undefined) {
            freq.set(sub, new RefNum(1));
        }
        else {
            m.num += 1;
        }
    }
    return freq;
}
function sort(sequence, length) {
    var freq = frequency(sequence, length);
    var keys = new Array();
    for (let k of freq.keys())
        keys.push(k);
    keys.sort((a, b) => (freq.get(b).num - freq.get(a).num));
    var n = sequence.length - length + 1;
    keys.forEach(key => {
        var count = (freq.get(key).num * 100 / n).toFixed(3);
        console.log(key + " " + count);
    });
    console.log("");
}
function find(haystack, needle) {
    var freq = frequency(haystack, needle.length);
    var m = freq.get(needle);
    var num = m ? m.num : 0;
    console.log(num + "\t" + needle);
}
function main() {
    var sequence = "";
    var reading = false;
    createInterface({ input: process.stdin, output: process.stdout })
        .on('line', line => {
        if (reading) {
            if (line[0] !== '>')
                sequence += line.toUpperCase();
        }
        else
            reading = line.substr(0, 6) === '>THREE';
    }).on('close', () => {
        sort(sequence, 1);
        sort(sequence, 2);
        find(sequence, 'GGT');
        find(sequence, 'GGTA');
        find(sequence, 'GGTATT');
        find(sequence, 'GGTATTTTAATT');
        find(sequence, 'GGTATTTTAATTTATAGT');
    });
}
main();
