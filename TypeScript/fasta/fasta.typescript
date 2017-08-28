/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Josh Goldfoot
   based on Node.js version by Roman Pletnev
   which was in turn based on C/Dart programs by Petr Prokhorenkov/Jos Hirth et al.
 */


/// <reference path="../node_modules/@types/node/index.d.ts" />

type Freq = { s: string; p: number; c: number; sc: number };

class Out {
    out_buffer_size: number;
    limit: number;
    buf: Buffer;
    ct: number;
    constructor() {
        this.out_buffer_size = 256 * 1024;
        this.limit = this.out_buffer_size - 2 * LINE_LEN - 1;
        this.buf = new Buffer(this.out_buffer_size);
        this.ct = 0;
    }
    flush(force: boolean) {
        if (this.ct > this.limit || force) {
            process.stdout.write(this.buf.toString(ENCODING, 0, this.ct));
            this.ct = 0;
        }
    }
}

var IM = 139968, IA = 3877, IC = 29573, last = 42;
var LINE_LEN = 60, NEW_LINE = 10, ENCODING = 'binary';
var out = new Out();

function random(): number {
    last = (last * IA + IC) % IM;
    return last / IM;
}

function repeat(alu: string, title: string, n : number): void {
    var len = alu.length, pos = 0;
    var buffer = new Buffer(alu + alu.substr(0, LINE_LEN), "ascii");
    out.buf.write(title, out.ct, title.length, ENCODING);
    out.ct += title.length;
    out.buf[out.ct++] = NEW_LINE;
    while (n) {
        var bytes = n > LINE_LEN ? LINE_LEN : n;
        out.flush(false);
        for (var i = 0; i < bytes; ++i) out.buf[out.ct++] = buffer[pos + i];
        out.buf[out.ct++] = NEW_LINE;
        pos += bytes;
        if (pos > len) pos -= len;
        n -= bytes;
    }
}

function make_cumulative(ac: Freq[]): void {
    var p = 0;
    for (var i = 0; i < ac.length; ++i) {
        p += ac[i].p;
        ac[i].c = p;
        ac[i].sc = ac[i].s.charCodeAt(0);
    }
}

function randomize(ac: Freq[], title: string, n: number): void {
    var len = alu.length, pos = 0;
    out.buf.write(title, out.ct, title.length, ENCODING);
    out.ct += title.length;
    out.buf[out.ct++] = NEW_LINE;
    while (n) {
        var bytes = n > LINE_LEN ? LINE_LEN : n;
        out.flush(false);
        for (var i = 0; i < bytes; ++i) {
            var r = random();
            for (var j = 0; j < ac.length; ++j) {
                if (r < ac[j].c) {
                    out.buf[out.ct++] = ac[j].sc;
                    break;
                } 
            }
            if (j === ac.length) out.buf[out.ct++] = ac[ac.length - 1].sc;
        }
        out.buf[out.ct++] = NEW_LINE;
        pos += bytes;
        if (pos > len) pos -= len;
        n -= bytes;
    }
}

var ac = [{ s: 'a', p: 0.27, c: 0, sc: 0 },
    { s: 'c', p: 0.12, c: 0, sc: 0 },
    { s: 'g', p: 0.12, c: 0, sc: 0 },
    { s: 't', p: 0.27, c: 0, sc: 0 },
    { s: 'B', p: 0.02, c: 0, sc: 0 },
    { s: 'D', p: 0.02, c: 0, sc: 0 },
    { s: 'H', p: 0.02, c: 0, sc: 0 },
    { s: 'K', p: 0.02, c: 0, sc: 0 },
    { s: 'M', p: 0.02, c: 0, sc: 0 },
    { s: 'N', p: 0.02, c: 0, sc: 0 },
    { s: 'R', p: 0.02, c: 0, sc: 0 },
    { s: 'S', p: 0.02, c: 0, sc: 0 },
    { s: 'V', p: 0.02, c: 0, sc: 0 },
    { s: 'W', p: 0.02, c: 0, sc: 0 },
    { s: 'Y', p: 0.02, c: 0, sc: 0 }];

var hs = [{ s: 'a', p: 0.3029549426680, c: 0, sc: 0 }, { s: 'c', p: 0.1979883004921, c: 0, sc: 0 },
    { s: 'g', p: 0.1975473066391, c: 0, sc: 0 }, { s: 't', p: 0.3015094502008, c: 0, sc: 0 }];

var alu = 'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTG'
    + 'GGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGA'
    + 'GACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAA'
    + 'AATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAAT'
    + 'CCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAAC'
    + 'CCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTG'
    + 'CACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA';

var n = process.argv[2] ? parseInt(process.argv[2]) : 512;

make_cumulative(ac);
make_cumulative(hs);

repeat(alu, '>ONE Homo sapiens alu', n * 2);
randomize(ac, '>TWO IUB ambiguity codes', n * 3);
randomize(hs, '>THREE Homo sapiens frequency', n * 5);
out.flush(true);
