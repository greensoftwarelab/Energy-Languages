/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   Contributed by Jesse Millikan
   Modified by Matt Baker
   Ported, modified, and parallelized by Roman Pletnev
*/

'use strict';

var rd = require('readline'), cp = require('child_process');

function RefNum(num){ this.num = num; }
RefNum.prototype.toString = function() { return this.num.toString(); }

function frequency(seq, length){
    var freq = new Map(), n = seq.length-length+1, key, cur, i = 0;
    for(; i<n; ++i){
        key = seq.substr(i, length);
        cur = freq.get(key);
        cur === undefined ? freq.set(key, new RefNum(1)) : ++cur.num;
    }
    return freq;
}

function sort(seq, length){
    var f = frequency(seq, length), keys = Array.from(f.keys()),
        n = seq.length-length+1, res = '';
    keys.sort((a, b)=>f.get(b)-f.get(a));
    for (var key of keys) res +=
        key.toUpperCase()+' '+(f.get(key)*100/n).toFixed(3)+'\n';
    res += '\n';
    return res;
}

function find(seq, s){
    var f = frequency(seq, s.length);
    return (f.get(s) || 0)+"\t"+s.toUpperCase()+'\n';
}

function master() {
    var workers = [];
    for (var i=1; i<5;++i) workers.push(
        cp.fork(__filename, [], {silent: true, env: {workerId: i}}));
    for (var w of workers) process.stdin.pipe(w.stdin);
    var jobs = workers.length, results = [];
    var messageHandler = function(i){
        return function(message){
            results[i] = message;
            if (!(--jobs)) {
                process.stdout.write(results.join(''));
                process.exit(0);
            }
        };
    };
    for (var i=0; i<workers.length; ++i)
        workers[i].on('message', messageHandler(i));
}

function worker(){
    var seq = '', reading = false;
    var lineHandler = function(line){
        if (reading) {
            if (line[0]!=='>') seq += line;
        } else reading = line.substr(0, 6)==='>THREE';
    };
    rd.createInterface(process.stdin, process.stdout)
        .on('line', lineHandler).on('close', function() {
            var res = '';
            switch (process.env.workerId) {
                case '1':
                    res += sort(seq, 1);
                    res += sort(seq, 2);
                    res += find(seq, "ggt");
                    break;
                case '2':
                    res += find(seq, "ggta");
                    res += find(seq, "ggtatt");
                    break;
                case '3':
                    res += find(seq, "ggtattttaatt");
                    break;
                case '4':
                    res += find(seq, "ggtattttaatttatagt");
                    break;
            }
            process.send(res);
            process.exit();
        });
}

process.env.workerId ? worker() : master();