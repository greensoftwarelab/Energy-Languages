/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   node.js version by Joe Farro
   TypeScript adaptation by Josh Goldfoot
*/
"use strict";
/// <reference path="../node_modules/@types/node/index.d.ts" />
class LinkedArray {
    constructor(prev) {
        this.prev = prev;
        this.next = null;
        this.pos = 0;
        this.data = [];
    }
}
const stdout = process.stdout;
const stdin = process.stdin;
const READ_SIZE = 16000;
const writeBuffer = new Buffer(READ_SIZE + READ_SIZE / 61 | 0);
var smap = [, , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , , ,
    ,
    , , , , 84, 86, 71, 72, , , 67, 68, , , 77, , 75, 78, , , , 89, 83, 65, 65, 66, 87, , 82, , , , , , , ,
    84, 86, 71, 72, , , 67, 68, , , 77, , 75, 78, , , , 89, 83, 65, 65, 66, 87, , 82];
const LA_LEN = 30;
const headLA = new LinkedArray(null);
let la = headLA;
let lnIdx = 0;
let lines = la.data;
let needHeader = true;
let headerPartial = '';
let isFirst = true;
var metaI;
var numLines;
function read() {
    let chunk = stdin.read(READ_SIZE);
    let isFinal = false;
    if (!chunk) {
        if (isFirst) {
            isFirst = false;
            return;
        }
        la.pos = lnIdx;
        reverse(la);
        return;
    }
    else if (chunk.length < READ_SIZE) {
        isFinal = true;
    }
    while (chunk) {
        while (true) {
            // if have read a partial header line, read the rest of it
            if (needHeader) {
                const headerEnds = chunk.indexOf('\n');
                console.log(headerPartial.toString() + chunk.slice(0, headerEnds).toString('ascii'));
                headerPartial = '';
                chunk = chunk.slice(headerEnds);
                needHeader = false;
            }
            const caretIdx = chunk.indexOf('>');
            if (caretIdx > -1) {
                // there is a caret in this chunk -- process the first part of
                // the chunk and then continue the `while (true)` loop to process
                // the next part of the chunk
                lines[lnIdx] = chunk.slice(0, caretIdx);
                // set chunk to the next section
                chunk = chunk.slice(caretIdx);
                la.pos = lnIdx + 1;
                reverse(la);
                // reset the data holders
                la = headLA;
                lines = la.data;
                la.pos = 0;
                lnIdx = 0;
                // check to see if the entire header line is here
                const headerEnds = chunk.indexOf('\n');
                if (headerEnds < 0) {
                    needHeader = true;
                    headerPartial = chunk;
                    break;
                }
                else {
                    // log the header
                    console.log(chunk.slice(0, headerEnds).toString('ascii'));
                    // continue processing the rest of the chunk
                    chunk = chunk.slice(headerEnds);
                }
            }
            else {
                if (lnIdx === LA_LEN) {
                    la.pos = LA_LEN;
                    la = la.next || (la.next = new LinkedArray(la));
                    lines = la.data;
                    lines[0] = chunk;
                    lnIdx = la.pos = 1;
                }
                else {
                    lines[lnIdx] = chunk;
                    lnIdx++;
                }
                break;
            }
        }
        chunk = stdin.read(READ_SIZE);
    }
    if (isFinal) {
        la.pos = lnIdx;
        reverse(la);
    }
}
function reverse(la_) {
    // reset the metaI and numLines in this section
    metaI = 0;
    numLines = 0;
    var la = la_, lines = la.data, lnIdx = la.pos - 1, line = lines[lnIdx];
    while (true) {
        reverseCompPrint(line);
        lnIdx--;
        line = lines[lnIdx];
        if (line) {
            continue;
        }
        la = la.prev;
        if (la === null) {
            break;
        }
        lines = la.data;
        lnIdx = la.pos;
        lnIdx--;
        line = lines[lnIdx];
    }
    if ((metaI - numLines) % 60 !== 0) {
        stdout.write('\n');
    }
}
function reverseCompPrint(line) {
    let _metaI = metaI;
    let _numLines = numLines;
    let count = 0;
    const target = writeBuffer;
    const len = line.length;
    const right = line.length - 1;
    let ileft = 0;
    let iright = 0;
    let c;
    while (iright < len) {
        c = line[right - iright];
        iright++;
        if (c == 10) {
            // skip linebreaks
            if (iright === len) {
                break;
            }
            c = line[right - iright];
            iright++;
        }
        target[ileft] = smap[c];
        ileft++;
        count++;
        if ((count + _metaI - _numLines) % 60 === 0) {
            // need a linebreak
            target[ileft] = 10;
            ileft++;
            count++;
            _numLines++;
        }
    }
    metaI = _metaI + count;
    numLines = _numLines;
    stdout.write(target.slice(0, count).toString('ascii'));
}
stdin.on('readable', read);
