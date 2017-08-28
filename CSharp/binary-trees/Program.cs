/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/ 

   contributed by Marek Safar  
   *reset*
   concurrency added by Peperud
*/

using System;
using System.Threading.Tasks;

class BinaryTrees
{
    const int MinDepth = 4;

    public static void Main(string[] args)
    {
        int n = 0;
        if (args.Length > 0) n = int.Parse(args[0]);

        int maxDepth = n < (MinDepth + 2) ? MinDepth + 2 : n;
        int stretchDepth = maxDepth + 1;

        var tcheck = new[]
        {
            Task.Run(() => TreeNode.BottomUpTree(stretchDepth).ItemCheck()),
            Task.Run(() => TreeNode.BottomUpTree(maxDepth).ItemCheck())
        };

        var results = new Task<string>[(maxDepth - MinDepth) / 2 + 1];

        for (int depth = MinDepth; depth <= maxDepth; depth += 2)
        {
            int iterations = 1 << (maxDepth - depth + MinDepth);

            int check = 0, safeDept = depth;

            results[(safeDept - MinDepth) / 2] = Task.Run(() =>
            {
                //for (var i = 1; i <= iterations; i++)
                //{
                //    check += (TreeNode.BottomUpTree(safeDept)).ItemCheck();
                //}

                // 
                // This is a somewhat silly, however overparallelizing towards 
                // the end gave me a little better numbers on average
                //
                int i = 1;
                while (i <= iterations)
                {
                    if (safeDept > 18)
                    {
                        var split = new[]
                        {
                            Task.Run(() => (TreeNode.BottomUpTree(safeDept)).ItemCheck()),
                            Task.Run(() => (TreeNode.BottomUpTree(safeDept)).ItemCheck())
                        };

                        i += 2;
                        Task.WaitAll(split);
                        check += split[0].Result + split[1].Result;
                    }
                    else
                    {
                        check += (TreeNode.BottomUpTree(safeDept)).ItemCheck();
                        i++;
                    }
                }

                return $"{iterations}\t trees of depth {safeDept}\t check: {check}";
            });
        }

        tcheck[0].Wait();
        Console.WriteLine("stretch tree of depth {0}\t check: {1}",
            stretchDepth, tcheck[0].Result);

        for (int i = 0; i < results.Length; i++)
        {
            results[i].Wait();
            Console.WriteLine(results[i].Result);
        }

        tcheck[1].Wait();
        Console.WriteLine("long lived tree of depth {0}\t check: {1}",
            maxDepth, tcheck[1].Result);
    }

    struct TreeNode
    {
        class Next
        {
            public TreeNode left, right;
        }

        private Next next;

        internal static TreeNode BottomUpTree(int depth)
        {
            if (depth > 0)
            {
                return new TreeNode(BottomUpTree(depth - 1), BottomUpTree(depth - 1));
            }
            else
            {
                return new TreeNode();
            }
        }

        TreeNode(TreeNode left, TreeNode right)
        {
            next = new Next
            {
                left = left,
                right = right
            };
        }

        internal int ItemCheck()
        {
            // if necessary deallocate here
            if (next == null)
            {
                return 1;
            }
            else
            {
                return 1 + next.left.ItemCheck() + next.right.ItemCheck();
            }
        }
    }
}

