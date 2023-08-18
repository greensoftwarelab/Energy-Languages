/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Isaac Gouy, transliterated from Oleg Mazurov's Java program
   concurrency fix and minor improvements by Peperud
*/

using System;
using System.Threading;
using System.Threading.Tasks;

class FannkuchRedux
{
    static int NCHUNKS = 150;
    static int CHUNKSZ;
    static int NTASKS;
    static int n;
    static int[] Fact;
    static int[] maxFlips;
    static int[] chkSums;
    static int taskId;

    int[] p, pp, count;

    const int INT_SIZE = 4;

    void FirstPermutation(int idx)
    {
        for (int i = 0; i < p.Length; ++i)
        {
            p[i] = i;
        }

        for (int i = count.Length - 1; i > 0; --i)
        {
            int d = idx / Fact[i];
            count[i] = d;
            idx = idx % Fact[i];

            Buffer.BlockCopy(p, 0, pp, 0, (i + 1) * INT_SIZE);

            for (int j = 0; j <= i; ++j)
            {
                p[j] = j + d <= i ? pp[j + d] : pp[j + d - i - 1];
            }
        }
    }

    bool NextPermutation()
    {
        int first = p[1];
        p[1] = p[0];
        p[0] = first;

        int i = 1;
        while (++count[i] > i)
        {
            count[i++] = 0;
            int next = p[0] = p[1];
            for (int j = 1; j < i; ++j)
            {
                p[j] = p[j + 1];
            }
            p[i] = first;
            first = next;
        }
        return true;
    }

    int CountFlips()
    {
        int flips = 1;
        int first = p[0];
        if (p[first] != 0)
        {
            Buffer.BlockCopy(p, 0, pp, 0, pp.Length * INT_SIZE);
            do
            {
                ++flips;
                for (int lo = 1, hi = first - 1; lo < hi; ++lo, --hi)
                {
                    int t = pp[lo];
                    pp[lo] = pp[hi];
                    pp[hi] = t;
                }
                int tp = pp[first];
                pp[first] = first;
                first = tp;
            } while (pp[first] != 0);
        }
        return flips;
    }

    void RunTask(int task)
    {
        int idxMin = task * CHUNKSZ;
        int idxMax = Math.Min(Fact[n], idxMin + CHUNKSZ);

        FirstPermutation(idxMin);

        int maxflips = 1;
        int chksum = 0;
        for (int i = idxMin; ;)
        {
            if (p[0] != 0)
            {
                int flips = CountFlips();

                if (maxflips < flips) maxflips = flips;

                chksum += i % 2 == 0 ? flips : -flips;
            }

            if (++i == idxMax)
            {
                break;
            }

            NextPermutation();
        }
        maxFlips[task] = maxflips;
        chkSums[task] = chksum;
    }

    public void Run()
    {
        p = new int[n];
        pp = new int[n];
        count = new int[n];

        int task;
        while ((task = Interlocked.Increment(ref taskId)) < NTASKS)
        {
            RunTask(task);
        }
    }


    static void Main(string[] args)
    {
        n = 7;
        if (args.Length > 0) n = int.Parse(args[0]);

        var nLen = n + 1;

        Fact = new int[nLen];
        Fact[0] = 1;
        for (int i = 1; i < nLen; ++i)
        {
            Fact[i] = Fact[i - 1] * i;
        }

        CHUNKSZ = (Fact[n] + NCHUNKS - 1) / NCHUNKS;
        NTASKS = (Fact[n] + CHUNKSZ - 1) / CHUNKSZ;
        maxFlips = new int[NTASKS];
        chkSums = new int[NTASKS];
        taskId = -1;

        int nthreads = Environment.ProcessorCount + 1;

        Task[] tasks = new Task[nthreads];
        for (int i = 0; i < nthreads; ++i)
        {
            tasks[i] = Task.Run(() =>
           {
               new FannkuchRedux().Run();
           });
        }
        Task.WaitAll(tasks);

        int res = 0, chk = 0;

        //
        // Would parallelizing this loop make any difference?
        //
        //for (int v = 0; v < NTASKS; v++)
        //{
        //    if (res < maxFlips[v]) res = maxFlips[v];
        //    chk += chkSums[v];
        //}

        Task[] t2 =
        {
            Task.Run(() =>
            {
                for (int v=0; v < NTASKS; v++)
                {
                    chk += chkSums[v];
                }
            }),

            Task.Run(() =>
            {
                for (int v=0; v < NTASKS; v++)
                {
                    if (res < maxFlips[v]) res = maxFlips[v];
                }
            })
        };

        Task.WaitAll(t2);

        Console.WriteLine("{0}\nPfannkuchen({1}) = {2}", chk, n, res);
    }
}