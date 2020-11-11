/*
 * The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 * 
 * modified by Mehmet D. AKIN
 * modified by Daryl Griffith
 */

import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;

public class fasta {

    static final int LINE_LENGTH = 60;
    static final int LINE_COUNT = 1024;
    static final NucleotideSelector[] WORKERS 
            = new NucleotideSelector[
                    Runtime.getRuntime().availableProcessors() > 1 
                    ? Runtime.getRuntime().availableProcessors() - 1 
                    : 1];
    static final AtomicInteger IN = new AtomicInteger();
    static final AtomicInteger OUT = new AtomicInteger();
    static final int BUFFERS_IN_PLAY = 6;
    static final int IM = 139968;
    static final int IA = 3877;
    static final int IC = 29573;
    static final float ONE_OVER_IM = 1f / IM;
    static int last = 42;

    public static void main(String[] args) {
        int n = 1000;

        if (args.length > 0) {
            n = Integer.parseInt(args[0]);
        }
        for (int i = 0; i < WORKERS.length; i++) {
            WORKERS[i] = new NucleotideSelector();
            WORKERS[i].setDaemon(true);
            WORKERS[i].start();
        }
        try (OutputStream writer = System.out;) {
            final int bufferSize = LINE_COUNT * LINE_LENGTH;

            for (int i = 0; i < BUFFERS_IN_PLAY; i++) {
                lineFillALU(
                        new AluBuffer(LINE_LENGTH, bufferSize, i * bufferSize));
            }
            speciesFillALU(writer, n * 2, ">ONE Homo sapiens alu\n");
            for (int i = 0; i < BUFFERS_IN_PLAY; i++) {
                writeBuffer(writer);
                lineFillRandom(new Buffer(true, LINE_LENGTH, bufferSize));
            }
            speciesFillRandom(writer
                    , n * 3
                    , ">TWO IUB ambiguity codes\n"
                    , true);
            for (int i = 0; i < BUFFERS_IN_PLAY; i++) {
                writeBuffer(writer);
                lineFillRandom(new Buffer(false, LINE_LENGTH, bufferSize));
            }
            speciesFillRandom(writer
                    , n * 5
                    , ">THREE Homo sapiens frequency\n"
                    , false);
            for (int i = 0; i < BUFFERS_IN_PLAY; i++) {
                writeBuffer(writer);
            }
        } catch (IOException ex) {
        }
     }

    private static void lineFillALU(AbstractBuffer buffer) {
        WORKERS[OUT.incrementAndGet() % WORKERS.length].put(buffer);
    }

    private static void bufferFillALU(OutputStream writer
            , int buffers) throws IOException {
        AbstractBuffer buffer;

        for (int i = 0; i < buffers; i++) {
            buffer = WORKERS[IN.incrementAndGet() % WORKERS.length].take();
            writer.write(buffer.nucleotides);
            lineFillALU(buffer);
        }
    }

    private static void speciesFillALU(final OutputStream writer
            , final int nChars
            , final String name) throws IOException {
        final int bufferSize = LINE_COUNT * LINE_LENGTH;
        final int bufferCount = nChars / bufferSize;
        final int bufferLoops = bufferCount - BUFFERS_IN_PLAY;
        final int charsLeftover = nChars - (bufferCount * bufferSize);

        writer.write(name.getBytes());
        bufferFillALU(writer, bufferLoops);
        if (charsLeftover > 0) {
            writeBuffer(writer);
            lineFillALU(
                    new AluBuffer(LINE_LENGTH
                            , charsLeftover
                            , nChars - charsLeftover));
        }
    }

    private static void lineFillRandom(Buffer buffer) {
        for (int i = 0; i < buffer.randoms.length; i++) {
            last = (last * IA + IC) % IM;
            buffer.randoms[i] = last * ONE_OVER_IM;
        }
        WORKERS[OUT.incrementAndGet() % WORKERS.length].put(buffer);
    }

    private static void bufferFillRandom(OutputStream writer
            , int loops) throws IOException {
        AbstractBuffer buffer;

        for (int i = 0; i < loops; i++) {
            buffer = WORKERS[IN.incrementAndGet() % WORKERS.length].take();
            writer.write(buffer.nucleotides);
            lineFillRandom((Buffer) buffer);
        }
    }

    private static void speciesFillRandom(final OutputStream writer
            , final int nChars
            , final String name
            , final boolean isIUB) throws IOException {
        final int bufferSize = LINE_COUNT * LINE_LENGTH;
        final int bufferCount = nChars / bufferSize;
        final int bufferLoops = bufferCount - BUFFERS_IN_PLAY;
        final int charsLeftover = nChars - (bufferCount * bufferSize);

        writer.write(name.getBytes());
        bufferFillRandom(writer, bufferLoops);
        if (charsLeftover > 0) {
            writeBuffer(writer);    
            lineFillRandom(new Buffer(isIUB, LINE_LENGTH, charsLeftover));
        }
    }

    private static void writeBuffer(OutputStream writer) throws IOException {
        writer.write(
                WORKERS[IN.incrementAndGet() % WORKERS.length]
                        .take()
                        .nucleotides);
    }

    public static class NucleotideSelector extends Thread {

        private final BlockingQueue<AbstractBuffer> 
                in = new ArrayBlockingQueue<>(BUFFERS_IN_PLAY);
        private final BlockingQueue<AbstractBuffer> 
                out = new ArrayBlockingQueue<>(BUFFERS_IN_PLAY);

        public void put(AbstractBuffer line) {
            try {
                in.put(line);
            } catch (InterruptedException ex) {
            }
        }

        @Override
        public void run() {
            AbstractBuffer line;

            try {
                for (;;) {
                    line = in.take();
                    line.selectNucleotides();
                    out.put(line);
                }
            } catch (InterruptedException ex) {
            }
        }

        public AbstractBuffer take() {
            try {
                return out.take();
            } catch (InterruptedException ex) {
            }
            return null;
        }
    }

    public abstract static class AbstractBuffer {

        final int LINE_LENGTH;
        final int LINE_COUNT;
        byte[] chars;
        final byte[] nucleotides;
        final int CHARS_LEFTOVER;

        public AbstractBuffer(final int lineLength, final int nChars) {
            LINE_LENGTH = lineLength;
            final int outputLineLength = lineLength + 1;
            LINE_COUNT = nChars / lineLength;
            CHARS_LEFTOVER = nChars % lineLength;
            final int nucleotidesSize 
                    = nChars + LINE_COUNT + (CHARS_LEFTOVER == 0 ? 0 : 1);
            final int lastNucleotide = nucleotidesSize - 1;

            nucleotides = new byte[nucleotidesSize];
            for (int i = lineLength
                    ; i < lastNucleotide
                    ; i += outputLineLength) {
                nucleotides[i] = '\n';
            }
            nucleotides[nucleotides.length - 1] = '\n';
        }

        public abstract void selectNucleotides();
    }

    public static class AluBuffer extends AbstractBuffer {

        final String ALU =
                "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
                + "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
                + "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
                + "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
                + "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
                + "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
                + "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";
        final int MAX_ALU_INDEX = ALU.length() - LINE_LENGTH;
        final int ALU_ADJUST = LINE_LENGTH - ALU.length();
        final int nChars;
        int charIndex;
        int nucleotideIndex;

        public AluBuffer(final int lineLength
                , final int nChars
                , final int offset) {
            super(lineLength, nChars);
            this.nChars = nChars;
            chars = (ALU + ALU.substring(0, LINE_LENGTH)).getBytes();
            charIndex = offset % ALU.length();
        }

        @Override
        public void selectNucleotides() {
            nucleotideIndex = 0;
            for (int i = 0; i < LINE_COUNT; i++) {
                ALUFillLine(LINE_LENGTH);
            }
            if (CHARS_LEFTOVER > 0) {
                ALUFillLine(CHARS_LEFTOVER);
            }
            charIndex = (charIndex + (nChars * (BUFFERS_IN_PLAY - 1))) 
                    % ALU.length();
        }

        private void ALUFillLine(final int charCount) {
            System.arraycopy(chars
                    , charIndex
                    , nucleotides
                    , nucleotideIndex
                    , charCount);
            charIndex += charIndex < MAX_ALU_INDEX ? charCount : ALU_ADJUST;
            nucleotideIndex += charCount + 1;
        }
    }

    public static class Buffer extends AbstractBuffer {

        final byte[] iubChars = new byte[]{
            'a', 'c', 'g', 't',
            'B', 'D', 'H', 'K',
            'M', 'N', 'R', 'S',
            'V', 'W', 'Y'};
        final double[] iubProbs = new double[]{
            0.27, 0.12, 0.12, 0.27,
            0.02, 0.02, 0.02, 0.02,
            0.02, 0.02, 0.02, 0.02,
            0.02, 0.02, 0.02,};
        final byte[] sapienChars = new byte[]{
            'a',
            'c',
            'g',
            't'};
        final double[] sapienProbs = new double[]{
            0.3029549426680,
            0.1979883004921,
            0.1975473066391,
            0.3015094502008};
        final float[] probs;
        final float[] randoms;
        final int charsInFullLines;

        public Buffer(final boolean isIUB
                , final int lineLength
                , final int nChars) {
            super(lineLength, nChars);
            double cp = 0;
            final double[] dblProbs = isIUB ? iubProbs : sapienProbs;

            chars = isIUB ? iubChars : sapienChars;
            probs = new float[dblProbs.length];
            for (int i = 0; i < probs.length; i++) {
                cp += dblProbs[i];
                probs[i] = (float) cp;
            }
            probs[probs.length - 1] = 2f;
            randoms = new float[nChars];
            charsInFullLines = (nChars / lineLength) * lineLength;
        }

        @Override
        public void selectNucleotides() {
            int i, j, m;
            float r;
            int k;

            for (i = 0, j = 0; i < charsInFullLines; j++) {
                for (k = 0; k < LINE_LENGTH; k++) {
                    r = randoms[i++];
                    for (m = 0; probs[m] < r; m++) {
                    }
                    nucleotides[j++] = chars[m];
                }
            }
            for (k = 0; k < CHARS_LEFTOVER; k++) {
                r = randoms[i++];
                for (m = 0; probs[m] < r; m++) {
                }
                nucleotides[j++] = chars[m];
            }
        }
    }
}