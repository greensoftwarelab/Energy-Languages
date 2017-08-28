/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
 * 
 * Regex-Redux
 * by Josh Goldfoot
 *
*/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Text.RegularExpressions;

class regexredux
{
    static void Main(string[] args)
    {

        // read FASTA sequence
        String sequence = Console.In.ReadToEnd();
        int initialLength = sequence.Length;

        // remove FASTA sequence descriptions and new-lines
        sequence = Regex.Replace(sequence, ">.*\n|\n", "");
        int codeLength = sequence.Length;

        Task<int> substitution = Task.Run(() => {
            // regex substitution
            string newseq = Regex.Replace(sequence, "tHa[Nt]", "<4>");
            newseq = Regex.Replace(newseq, "aND|caN|Ha[DS]|WaS", "<3>");
            newseq = Regex.Replace(newseq, "a[NSt]|BY", "<2>");
            newseq = Regex.Replace(newseq, "<[^>]*>", "|");
            newseq = Regex.Replace(newseq, "\\|[^|][^|]*\\|" , "-");
            return newseq.Length;
        });

        // divide large sequence into chunks (one per core) and search each in parallel
        int[][] sums = Chunks(sequence).AsParallel().Select(CountRegexes).ToArray();

        var variants = Variants.variantsCopy();
        for (int i = 0; i < 9; i++)
            Console.WriteLine("{0} {1}", variants[i], sums.Sum(a => a[i]));

        Console.WriteLine("\n{0}\n{1}\n{2}",
           initialLength, codeLength, substitution.Result);
    }

    private static IEnumerable<string> Chunks(string sequence)
    {
        int numChunks = Environment.ProcessorCount;
        int start = 0;
        int chunkSize = sequence.Length / numChunks;
        while (--numChunks >= 0)
        {
            if (numChunks > 0)
                yield return sequence.Substring(start, chunkSize);
            else
                yield return sequence.Substring(start);
            start += chunkSize;
        }
    }

    private static int[] CountRegexes(string chunk)
    {
        // regex match
        int[] counts = new int[9];
        string[] variants = Variants.variantsCopy();

        for (int i = 0; i < 9; i++)
            for (var m = Regex.Match(chunk, variants[i]); m.Success; m = m.NextMatch()) counts[i]++;
        return counts;
    }
}

public class Variants
{
    public static string[] variantsCopy()
    {
        return new string[] {
          "agggtaaa|tttaccct"
         ,"[cgt]gggtaaa|tttaccc[acg]"
         ,"a[act]ggtaaa|tttacc[agt]t"
         ,"ag[act]gtaaa|tttac[agt]ct"
         ,"agg[act]taaa|ttta[agt]cct"
         ,"aggg[acg]aaa|ttt[cgt]ccct"
         ,"agggt[cgt]aa|tt[acg]accct"
         ,"agggta[cgt]a|t[acg]taccct"
         ,"agggtaa[cgt]|[acg]ttaccct"
        };
    }
}