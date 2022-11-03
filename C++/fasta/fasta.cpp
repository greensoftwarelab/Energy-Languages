/* The Computer Language Benchmarks Game
 http://benchmarksgame.alioth.debian.org/
 
 converted to C++ from D by Rafal Rusin
 modified by Vaclav Haisman
 modified by The Anh to compile with g++ 4.3.2
 modified by Branimir Maksimovic
 modified by Kim Walisch
 modified by Tavis Bohne
 made multithreaded by Jeff Wofford on the model of fasta C gcc #7 and fasta Rust #2
 
 compiles with gcc fasta.cpp -std=c++11 -O2
 */

#include <algorithm>
#include <array>
#include <vector>
#include <thread>
#include <mutex>
#include <iostream>
#include <numeric>
#include <functional>

struct IUB
{
   float p;
   char c;
};

const std::string alu =
{
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"
};

std::array<IUB,15> iub =
{{
   { 0.27f, 'a' },
   { 0.12f, 'c' },
   { 0.12f, 'g' },
   { 0.27f, 't' },
   { 0.02f, 'B' },
   { 0.02f, 'D' },
   { 0.02f, 'H' },
   { 0.02f, 'K' },
   { 0.02f, 'M' },
   { 0.02f, 'N' },
   { 0.02f, 'R' },
   { 0.02f, 'S' },
   { 0.02f, 'V' },
   { 0.02f, 'W' },
   { 0.02f, 'Y' }
}};

std::array<IUB, 4> homosapiens =
{{
   { 0.3029549426680f, 'a' },
   { 0.1979883004921f, 'c' },
   { 0.1975473066391f, 'g' },
   { 0.3015094502008f, 't' }
}};

const int IM = 139968;
const float IM_RECIPROCAL = 1.0f / IM;

uint32_t gen_random()
{
   static const int IA = 3877, IC = 29573;
   static int last = 42;
   last = (last * IA + IC) % IM;
   return last;
}

char convert_trivial(char c)
{
   return c;
}

template<class iterator_type>
class repeat_generator_type {
public:
   using result_t = char;
   
   repeat_generator_type(iterator_type first, iterator_type last)
   : first(first), current(first), last(last)
   { }
   result_t operator()()
   {
      if (current == last)
         current = first;
      iterator_type p = current;
      ++current;
      return *p;
   }
private:
   iterator_type first;
   iterator_type current;
   iterator_type last;
};
template<class iterator_type>
repeat_generator_type<iterator_type>
make_repeat_generator(iterator_type first, iterator_type last)
{return repeat_generator_type<iterator_type>(first, last);}

template<class iterator_type>
char convert_random(uint32_t random, iterator_type begin, iterator_type end)
{
   const float p = random * IM_RECIPROCAL;
   auto result = std::find_if(begin, end, [p] (IUB i) { return p <= i.p; });
   return result->c;
}

char convert_IUB(uint32_t random)
{
   return convert_random(random, iub.begin(), iub.end() );
}

char convert_homosapiens(uint32_t random)
{
   return convert_random(random, homosapiens.begin(), homosapiens.end());
}

template<class iterator_type>
class random_generator_type {
public:
   using result_t = uint32_t;
   
   random_generator_type(iterator_type first, iterator_type last)
   : first(first), last(last)
   { }
   result_t operator()()
   {
      return gen_random();
   }
private:
   iterator_type first;
   iterator_type last;
};
template<class iterator_type>
random_generator_type<iterator_type>
make_random_generator(iterator_type first, iterator_type last)
{return random_generator_type<iterator_type>(first, last);}

template<class iterator_type>
void make_cumulative(iterator_type first, iterator_type last)
{
   std::partial_sum(first, last, first,
                [] (IUB l, IUB r) -> IUB { r.p += l.p; return r; });
}

const size_t CHARS_PER_LINE = 60;
const size_t CHARS_PER_LINE_INCL_NEWLINES = CHARS_PER_LINE + 1;
const size_t LINES_PER_BLOCK = 1024;
const size_t VALUES_PER_BLOCK = CHARS_PER_LINE * LINES_PER_BLOCK;
const size_t CHARS_PER_BLOCK_INCL_NEWLINES = CHARS_PER_LINE_INCL_NEWLINES * LINES_PER_BLOCK;

const unsigned THREADS_TO_USE = std::max( 1U, std::min( 4U, std::thread::hardware_concurrency() ));

#define LOCK( mutex ) std::lock_guard< decltype( mutex ) > guard_{ mutex };

std::mutex g_fillMutex;
size_t g_fillThreadIndex = 0;
size_t g_totalValuesToGenerate = 0;

template<class iterator_type, class generator_type>
size_t fillBlock(size_t currentThread, iterator_type begin, generator_type& generator)
{
   while( true )
   {
      LOCK(g_fillMutex);
      if(currentThread == g_fillThreadIndex)
      {
         // Select the next thread for this work.
         ++g_fillThreadIndex;
         if(g_fillThreadIndex >= THREADS_TO_USE)
         {
            g_fillThreadIndex = 0;
         }
         
         // Do the work.
         const size_t valuesToGenerate = std::min(g_totalValuesToGenerate, VALUES_PER_BLOCK);
         g_totalValuesToGenerate -= valuesToGenerate;
         
         for(size_t valuesRemaining = 0; valuesRemaining < valuesToGenerate; ++valuesRemaining)
         {
            *begin++ = generator();
         }
         
         return valuesToGenerate;
      }
   }
}

template<class BlockIter, class CharIter, class converter_type>
size_t convertBlock(BlockIter begin, BlockIter end, CharIter outCharacter, converter_type& converter)
{
   const auto beginCharacter = outCharacter;
   size_t col = 0;
   for( ; begin != end; ++begin)
   {
      const uint32_t random = *begin;
      
      *outCharacter++ = converter(random);
      if(++col >= CHARS_PER_LINE)
      {
         col = 0;
         *outCharacter++ = '\n';
      }
   }
   //Check if we need to end the line
   if(0 != col)
   {
      //Last iteration didn't end the line, so finish the job.
      *outCharacter++ = '\n';
   }
   
   return std::distance(beginCharacter, outCharacter);
}

std::mutex g_outMutex;
size_t g_outThreadIndex = -1;

template<class iterator_type>
void writeCharacters(size_t currentThread, iterator_type begin, size_t count)
{
   while(true)
   {
      LOCK(g_outMutex);
      if(g_outThreadIndex == -1 || currentThread == g_outThreadIndex)
      {
         // Select the next thread for this work.
         g_outThreadIndex = currentThread + 1;
         if(g_outThreadIndex >= THREADS_TO_USE)
         {
            g_outThreadIndex = 0;
         }
         
         // Do the work.
         std::fwrite( begin, count, 1, stdout );
         return;
      }
   }
}


template<class generator_type, class converter_type>
void work(size_t currentThread, generator_type& generator, converter_type& converter)
{
   std::array< typename generator_type::result_t, VALUES_PER_BLOCK > block;
   std::array< char, CHARS_PER_BLOCK_INCL_NEWLINES > characters;
   
   while(true)
   {
      const auto bytesGenerated = fillBlock(currentThread, block.begin(), generator);
      
      if( bytesGenerated == 0 )
      {
         break;
      }
      
      const auto charactersGenerated = convertBlock(block.begin(), block.begin() + bytesGenerated, characters.begin(), converter);
      
      writeCharacters(currentThread, characters.begin(), charactersGenerated);
   }
}

template <class generator_type, class converter_type >
void make(const char* desc, int n, generator_type generator, converter_type converter) {
   std::cout << '>' << desc << '\n';
   
   g_totalValuesToGenerate = n;
   g_outThreadIndex = -1;
   g_fillThreadIndex = 0;
   
   std::vector< std::thread > threads(THREADS_TO_USE - 1);
   for(size_t i = 0; i < threads.size(); ++i)
   {
      threads[ i ] = std::thread{ std::bind( &work< generator_type, converter_type >, i, std::ref(generator), std::ref(converter)) };
   }
   
   work(threads.size(), generator, converter);
   
   for(auto& thread : threads)
   {
      thread.join();
   }
}

int main(int argc, char *argv[])
{
   int n = 1000;
   if (argc < 2 || (n = std::atoi(argv[1])) <= 0) {
      std::cerr << "usage: " << argv[0] << " length\n";
      return 1;
   }
   
   make_cumulative(iub.begin(), iub.end());
   make_cumulative(homosapiens.begin(), homosapiens.end());
   
   make("ONE Homo sapiens alu"      , n * 2,
       make_repeat_generator(alu.begin(), alu.end()),
       &convert_trivial );
   make("TWO IUB ambiguity codes"   , n * 3,
       make_random_generator(iub.begin(), iub.end()),
       &convert_IUB );
   make("THREE Homo sapiens frequency", n * 5,
       make_random_generator(homosapiens.begin(), homosapiens.end()),
       &convert_homosapiens );
   return 0;
}
