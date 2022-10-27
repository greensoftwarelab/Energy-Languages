/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Artem Zaytsev
 */

'use strict';

class RandomGenerator
{
    constructor()
    {
        this.last = 42;
    }

    generate()
    {
        this.last = ((this.last * 3877 + 29573) % 139968) | 0;
        return this.last / 139968;
    }
}

class StringRepeater
{
    constructor(string, resultLength, lineLength)
    {
        this.remainingLength = (resultLength + Math.floor(resultLength / lineLength)) | 0;
        this.buffers = StringRepeater._precalculateBuffers(16 * 1024, string, lineLength);
        this.bufferIndex = 0;
    }

    static _precalculateBuffers(preferredBufferSize, string, lineLength)
    {
        const splitterRegexp = new RegExp('(.{1,' + lineLength + '})', 'g');

        const bufSize = Math.max(preferredBufferSize, lineLength * 2) | 0;

        const baseLength = (Math.ceil(bufSize / string.length) * string.length) | 0;
        const base = string.repeat(baseLength / string.length);

        var tail = '';

        const buffers = [];

        do {
            const lines = (tail + base).match(splitterRegexp);

            lines[0] = lines[0].slice(tail.length);

            tail = lines[lines.length - 1];

            if (tail.length === lineLength) {
                lines[lines.length - 1] += '\n';
            }

            buffers.push(Buffer.from(lines.join('\n'), 'ascii'));
        } while (tail.length !== lineLength);

        return buffers;
    }

    read()
    {
        if (!this.remainingLength) {
            return null;
        }

        const chunk = this.buffers[this.bufferIndex];

        this.bufferIndex = (this.bufferIndex + 1) % this.buffers.length;

        if (chunk.length > this.remainingLength) {
            const slice = chunk.slice(0, this.remainingLength)

            this.remainingLength = 0;

            return slice;
        }

        this.remainingLength -= chunk.length;

        return chunk;
    }
}

class DNAGenerator
{
    constructor(probs, randomGenerator, resultLength, lineLength)
    {
        this.cumulativeProbs = DNAGenerator._makeCumulative(probs);

        this.randomGenerator = randomGenerator;
        this.lineLength = lineLength | 0;
        this.remainingLength = (resultLength + Math.floor(resultLength / lineLength));

        const bufferSize = Math.ceil(64 * 1024 / (lineLength + 1)) * (lineLength + 1);

        this.buffer = Buffer.allocUnsafe(bufferSize);
    }

    static _makeCumulative(probs)
    {
        const result = [];
        var sum = 0;

        for (const o of probs) {
            result.push({
                code: o.s.charCodeAt(0),
                cumulativeProb: sum + o.p
            });

            sum += o.p;
        }

        return result;
    }

    _randomCode()
    {
        const r = this.randomGenerator.generate();

        for (var i = 0; i < this.cumulativeProbs.length - 1; i++) {
            const p = this.cumulativeProbs[i];

            if (r < p.cumulativeProb) {
                return p.code;
            }
        }

        return this.cumulativeProbs[this.cumulativeProbs.length - 1].code;
    }

    read()
    {
        if (!this.remainingLength) {
            return null;
        }

        const remaining = Math.min(this.remainingLength, this.buffer.length) | 0;

        const ll = this.lineLength + 1;

        for (var i = 1; i <= remaining; ++i) {
            if ((i % ll) === 0) {
                this.buffer[i - 1] = 0x0a;
            } else {
                this.buffer[i - 1] = this._randomCode();
            }
        }

        this.remainingLength -= remaining;

        if (remaining < this.buffer.length) {
            return this.buffer.slice(0, remaining);
        }

        return this.buffer;
    }
}

class Fasta
{
    constructor(outputStream)
    {
        this.lineLength = 60;
        this.outputStream = outputStream;
    }

    _readGenerator(gen)
    {
        return new Promise((resolve, reject) => {
            var needEol = false;

            const next = () => {
                const chunk = gen.read();

                if (!chunk) {
                    if (needEol) {
                        this.outputStream.write('\n', resolve);
                    } else {
                        resolve();
                    }
                    return;
                }

                needEol = chunk[chunk.length - 1] !== 0x0a;

                this.outputStream.write(chunk, next);
            };

            next();
        });
    }

    repeat(alu, title, length)
    {
        const repeater = new StringRepeater(alu, length, this.lineLength);

        this.outputStream.write(title + '\n');

        return this._readGenerator(repeater);
    }

    generate(probs, title, randomGenerator, length)
    {
        const g = new DNAGenerator(probs, randomGenerator, length, 60);

        this.outputStream.write(title + '\n');

        return this._readGenerator(g);

    }
}

const ac = [{s: 'a', p: 0.27}, {s: 'c', p: 0.12}, {s: 'g', p: 0.12},
    {s: 't', p: 0.27}, {s: 'B', p: 0.02}, {s: 'D', p: 0.02},
    {s: 'H', p: 0.02}, {s: 'K', p: 0.02}, {s: 'M', p: 0.02},
    {s: 'N', p: 0.02}, {s: 'R', p: 0.02}, {s: 'S', p: 0.02},
    {s: 'V', p: 0.02}, {s: 'W', p: 0.02}, {s: 'Y', p: 0.02}];

const hs = [{s: 'a', p: 0.3029549426680}, {s: 'c', p: 0.1979883004921},
    {s: 'g', p: 0.1975473066391}, {s: 't', p: 0.3015094502008}];

const fasta = new Fasta(process.stdout);
const n = parseInt(process.argv[2]);
const random = new RandomGenerator;

fasta.repeat(
    'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTG'
        +'GGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGA'
        +'GACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAA'
        +'AATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAAT'
        +'CCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAAC'
        +'CCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTG'
        +'CACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA',
    '>ONE Homo sapiens alu',
    n * 2
).then(() => {
    return fasta.generate(ac, '>TWO IUB ambiguity codes', random, n * 3);
}).then(() => {
    return fasta.generate(hs, '>THREE Homo sapiens frequency', random, n * 5);
});