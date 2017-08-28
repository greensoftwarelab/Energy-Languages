/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   Contributed by Peperud

   Attempt at introducing concurrency.
   Ideas and code borrowed from various other contributions and places. 

   TODO 
        Better way to reverse complement the block's body without having 
        to flatten it first.
*/

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;

static class revcomp
{
    static BlockingCollection<byte[]> readQue = new BlockingCollection<byte[]>();
    static BlockingCollection<RCBlock> inQue = new BlockingCollection<RCBlock>();
    static BlockingCollection<RCBlock> outQue = new BlockingCollection<RCBlock>();

    static readonly int READER_BUFFER_SIZE = 1024 * 1024 * 16;

    static readonly byte[] comp = new byte[256];
    static readonly byte LF = 10;
    const string Seq = "ABCDGHKMRTVYabcdghkmrtvy";
    const string Rev = "TVGHCDMKYABRTVGHCDMKYABR";

    static void Main(string[] args)
    {
        Task.Run(() => Reader());

        Task.Run(() => Parser());

        InitComplements();

        var reverser = Task.Run(() => Reverser());
        
        var writer = Task.Run(() => Writer());

        reverser.Wait();
        outQue.CompleteAdding();

        writer.Wait();
    }

    static void Reader()
    {
        using (var inS = Console.OpenStandardInput())
        {
            byte[] buf;
            int bytesRead;

            do
            {
                buf = new byte[READER_BUFFER_SIZE];
                bytesRead = inS.Read(buf, 0, READER_BUFFER_SIZE);
                if (bytesRead > 0)
                {
                    if (bytesRead == READER_BUFFER_SIZE)
                    {
                        readQue.Add(buf);
                    }
                    else
                    {
                        var tmp = new byte[bytesRead];
                        Buffer.BlockCopy(buf, 0, tmp, 0, bytesRead);
                        readQue.Add(tmp);
                    }
                }
            } while (bytesRead == READER_BUFFER_SIZE);

            readQue.CompleteAdding();

        }
    }

    static void Parser()
    {
        var rdr = new RCBlockReader(readQue);
        var rcBlock = rdr.Read();
        while (rcBlock != null)
        {
            inQue.Add(rcBlock);
            rcBlock = rdr.Read();
        }
        inQue.CompleteAdding();
    }

    static void Reverser()
    {
        try
        {
            while (!inQue.IsCompleted)
            {
                var block = inQue.Take();
                block.ReverseAndComplement();
                outQue.Add(block);
            }
        }
        catch (InvalidOperationException) { }
    }

    static void Writer()
    {
        using (var outS = Console.OpenStandardOutput())
        {
            try
            {
                while (!outQue.IsCompleted)
                {
                    var block = outQue.Take();
                    block.Write(outS);
                }
            }
            catch (InvalidOperationException) { }
        }
    }

    static void InitComplements()
    {
        for (byte i = 0; i < 255; i++)
        {
            comp[i] = i;
        }
        for (int i = 0; i < Seq.Length; i++)
        {
            comp[(byte)Seq[i]] = (byte)Rev[i];
        }
        comp[LF] = 0;
        comp[(byte)' '] = 0;
    }

    static byte[] FlattenList(List<byte[]> lineBuffer)
    {
        int cnt = lineBuffer.Count;

        // Should we calculate the total size as we go, 
        // or should we do it here (in the dedicated CPU bound task)?

        int totalSize = 0;
        for (var i = 0; i < cnt; i++)
        {
            totalSize += lineBuffer[i].Length;
        }

        byte[] result = new byte[totalSize];

        int pos = 0;

        for (var i = 0; i < cnt; i++)
        {
            Buffer.BlockCopy(lineBuffer[i], 0, result, pos, lineBuffer[i].Length);
            pos += lineBuffer[i].Length;
        }

        return result;
    }

    class RCBlock
    {
        public List<byte[]> Title;
        public List<byte[]> Data;
        public byte[] FlatData;
        public byte[] FlatTitle;

        internal void ReverseAndComplement()
        {
            var tt = new []
            {
                Task.Run(() => FlattenData()),
                Task.Run(() => FlattenTitle())
            };
            Task.WaitAll(tt);

            int i = 0, j = FlatData.Length - 1;
            byte ci, cj;

            while (i < j)
            {
                ci = FlatData[i];
                if (ci == 10)
                {
                    i++;
                    ci = FlatData[i];
                }
                cj = FlatData[j];
                if (cj == 10)
                {
                    j--;
                    cj = FlatData[j];
                }
                FlatData[i] = comp[cj];
                FlatData[j] = comp[ci];
                i++;
                j--;
            }
        }

        void FlattenTitle()
        {
            FlatTitle = FlattenList(Title);
        }

        void FlattenData()
        {
            FlatData = FlattenList(Data);
        }

        internal void Write(Stream s)
        {
            s.Write(FlatTitle, 0, FlatTitle.Length);
            s.WriteByte(10);
            s.Write(FlatData, 0, FlatData.Length);
        }
    }

    class RCBlockReader
    {
        BlockingCollection<byte[]> readQue;
        static readonly byte GT = (byte)'>';     
        byte[] byteBuffer;
        int bytePos, bytesRead;

        public RCBlockReader(BlockingCollection<byte[]> readQue)
        {
            this.readQue = readQue;
        }

        public RCBlock Read()
        {
            var title = ReadLine(LF);
            if (title == null)
            {
                return null;
            }

            return new RCBlock { Title = title, Data = ReadLine(GT, true) };
        }

        private List<byte[]> ReadLine(byte lineSeparator, bool dontAdvanceBytePos = false)
        {
            if (bytePos == bytesRead && ReadToBuffer() == 0)
            {
                return null;
            }
            if (bytesRead == 0) return null;

            List<byte[]> lineBuffer = null;
            int num;
            byte c;

            while (true)
            {
                num = bytePos;
                do
                {
                    c = byteBuffer[num];
                    if (c == lineSeparator)
                    {
                        byte[] result = new byte[num - bytePos];
                        Buffer.BlockCopy(byteBuffer, bytePos, result, 0, result.Length);
                        bytePos = num + (dontAdvanceBytePos ? 0 : 1);

                        if (lineBuffer != null)
                        {
                            lineBuffer.Add(result);
                        }
                        else
                        {
                            lineBuffer = new List<byte[]> { result };
                        }
                        return lineBuffer;
                    }
                    num++;
                } while (num < bytesRead);

                num = bytesRead - bytePos;
                if (lineBuffer == null)
                {
                    lineBuffer = new List<byte[]>();
                }
                var tmp = new byte[num];
                Buffer.BlockCopy(byteBuffer, bytePos, tmp, 0, num);

                lineBuffer.Add(tmp);
                if (ReadToBuffer() <= 0)
                {
                    return lineBuffer;
                }
            }
        }

        private int ReadToBuffer()
        {
            try
            {
                byteBuffer = readQue.Take();
                bytePos = 0;
                bytesRead = byteBuffer.Length;
                return byteBuffer.Length;
            }
            catch (InvalidOperationException)
            {
                byteBuffer = null;
                bytesRead = 0;
                return 0;
            }
        }
    }
}