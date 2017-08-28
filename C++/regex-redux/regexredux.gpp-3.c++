/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   regex-dna program contributed by Alexey Zolotov
   modified by Vaclav Zeman
   converted from regex-dna program
*/

#include <boost/regex.hpp>
#include <cassert>
#include <iostream>
#include <cstdio>

using namespace std;

const std::size_t BUFSIZE = 1024;
const boost::regex::flag_type re_flags = boost::regex::perl;

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
    boost::regex re1 (">[^\\n]+\\n|[\\n]", re_flags);
    boost::regex_replace (str, re1, "").swap (str);
    len2 = str.length();

    out = str;
    int counts[pattern1_count] = { 0 };

    #pragma omp parallel sections
    {
    #pragma omp section
        #pragma omp parallel for
        for (int i = 0; i < pattern1_count; i++)
        {
            boost::regex pat(pattern1[i], re_flags);
            boost::smatch m;
            std::string::const_iterator start = str.begin (), end = str.end (); 
            while (boost::regex_search (start, end, m, pat))
            {
                ++counts[i];
                start += m.position () + m.length ();
            }
            
            
        }
        #pragma omp section
        for (int i = 0; i < (int)(sizeof(pattern2) / sizeof(string)); i += 2)
        {
            boost::regex re (pattern2[i], re_flags);
            boost::regex_replace (out, re, pattern2[i + 1]).swap (out);
        }
    }

    for (int i = 0; i != pattern1_count; ++i)
      cout << pattern1[i] << " " << counts[i] << "\n";

    cout << "\n";
    cout << len1 << "\n";
    cout << len2 << "\n";
    cout << out.length() << endl;
}