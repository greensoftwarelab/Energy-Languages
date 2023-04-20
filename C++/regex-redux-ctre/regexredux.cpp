/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   regex-dna program contributed by Alexey Zolotov
   modified by Vaclav Zeman
   converted from regex-dna program
*/

#include <re2/re2.h>
#include <re2/stringpiece.h>
#include <cassert>
#include <iostream>
#include <cstdio>

using namespace std;

const std::size_t BUFSIZE = 1024;

int main(void)
{
    string str, out;
    int len1, len2;
    int read_size;
    char *buf;

    char const * pattern1[] = {
        "agggtaaa|tttaccct",
        "[cgt]gggtaaa|tttaccc[acg]",
        "a[act]ggtaaa|tttacc[agt]t",
        "ag[act]gtaaa|tttac[agt]ct",
        "agg[act]taaa|ttta[agt]cct",
        "aggg[acg]aaa|ttt[cgt]ccct",
        "agggt[cgt]aa|tt[acg]accct",
        "agggta[cgt]a|t[acg]taccct",
        "agggtaa[cgt]|[acg]ttaccct"
    };

    const int pattern1_count = (int)(sizeof(pattern1) / sizeof(pattern1[0]));

    string const pattern2[] = {
        "tHa[Nt]", "<4>", "aND|caN|Ha[DS]|WaS", "<3>", "a[NSt]|BY", "<2>",
        "<[^>]*>", "|", "\\|[^|][^|]*\\|", "-"
    };

    fseek(stdin, 0, SEEK_END);
    read_size = ftell(stdin);
    assert(read_size > 0);

    str.resize (read_size);
    rewind(stdin);
    read_size = fread(&str[0], 1, read_size, stdin);
    assert(read_size);

    len1 = str.length();
    RE2 re1(">[^\\n]+\\n|[\\n]");
    RE2::GlobalReplace(&str, re1, "");
    len2 = str.length();

    out = str;
    int counts[pattern1_count] = { 0 };

    #pragma omp parallel sections
    {
    #pragma omp section
        #pragma omp parallel for
        for (int i = 0; i < pattern1_count; i++)
        {
            RE2 pat(pattern1[i]);
            re2::StringPiece input(str);
            while (RE2::FindAndConsume(&input, pat)) {
                ++counts[i];
            }
        }
        #pragma omp section
        for (int i = 0; i < (int)(sizeof(pattern2) / sizeof(string)); i += 2)
        {
            RE2 re(pattern2[i]);
            RE2::GlobalReplace(&out, re, pattern2[i + 1]);
        }
    }

    for (int i = 0; i != pattern1_count; ++i)
      cout << pattern1[i] << " " << counts[i] << "\n";

    cout << "\n";
    cout << len1 << "\n";
    cout << len2 << "\n";
    cout << out.length() << endl;
}