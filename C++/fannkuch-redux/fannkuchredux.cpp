// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Contributed by Dave Compton
// Based on "fannkuch-redux C gcc #5", contributed by Jeremy Zerfas
// which in turn was based on the Ada program by Jonathan Parker and 
// Georg Bauhaus which in turn was based on code by Dave Fladebo, 
// Eckehard Berns, Heiner Marxen, Hongwei Xi, and The Anh Tran and 
// also the Java program by Oleg Mazurov.

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

static int64_t fact[32];

void initializeFact(int n)
{
    fact[0] = 1;
    for (auto i = 1; i <= n; ++i)
        fact[i] = i * fact[i - 1];
}

class Permutation 
{
  public:
    Permutation(int n, int64_t start);
    void advance();
    int64_t countFlips() const;

  private:
     vector <int> count;
     vector <int8_t> current;

};

// 
// Initialize the current value of a permutation
// and the cycle count values used to advance .
// 
Permutation::Permutation(int n, int64_t start)
{
    count.resize(n);
    current.resize(n);

    // Initialize count 
    for (auto i = n - 1; i >= 0; --i) 
    {
        auto d = start / fact[i];
        start = start % fact[i];
        count[i] = d;
    }

    // Initialize current.
    for (auto i = 0; i < n; ++i)
        current[i] = i;

    for (auto i = n - 1; i >= 0; --i) 
    {
        auto d = count[i];
        auto b = current.begin();
        rotate(b, b + d, b + i + 1);
    }
}

//
// Advance the current permutation to the next in sequence.
// 
void Permutation::advance()
{
    for (auto i = 1; ;++i) 
    {
        // Tried using std::rotate here but that was slower.
        auto first = current[0];
        for (auto j = 0; j < i; ++j)
            current[j] = current[j + 1];
        current[i] = first;

        ++(count[i]);
        if (count[i] <= i)
            break;
        count[i] = 0;
    }
}

//
// Count the flips required to flip 0 to the front of the vector.
//
// Other than minor cosmetic changes, the following routine is
// basically lifted from "fannkuch-redux C gcc #5"
//
inline int64_t Permutation::countFlips() const
{
    const auto n = current.size();
    auto flips = 0;
    auto first = current[0];
    if (first > 0) 
    {
        flips = 1;

        int8_t temp[n];
        // Make a copy of current to work on. 
        for (size_t i = 0; i < n; ++i)
            temp[i] = current[i];


        // Flip temp until the element at the first index is 0
        for (; temp[first] > 0; ++flips) 
        {
            // Record the newFirst and restore the old
            // first at its new flipped position.
            const int8_t newFirst = temp[first];
            temp[first] = first;

            if (first > 2) 
            {
                int64_t low = 1, high = first - 1;
                do 
                {
                    swap(temp[low], temp[high]);
                    if (!(low + 3 <= high && low < 16))
                        break;
                    ++low;
                    --high;
                } while (1);
            }
            // Update first to newFirst that we recorded earlier.
            first = newFirst;
        }
    }
    return flips;
}

int main(int argc, char **argv)
{
    const auto n = atoi(argv[1]);

    // Compute some factorials for later use.
    initializeFact(n);

    // blockCount works best if it is set to a multiple of the number
    // of CPUs so that the same number of blocks gets distributed to
    // each cpu.  The computer used for development (Intel i7-4700MQ)
    // had 8 "CPU"s (4 cores with hyperthreading) so 8, 16 and 24 
    // all worked well.

    auto blockCount = 24;
    if (blockCount > fact[n])
        blockCount = 1;
    const int64_t blockLength = fact[n] / blockCount;

    int64_t maxFlips = 0, checksum = 0;

    // Iterate over each block.
    #pragma omp parallel for \
        reduction(max:maxFlips) \
        reduction(+:checksum)

    for (int64_t blockStart = 0;
         blockStart < fact[n]; 
         blockStart += blockLength) 
    {
        // first permutation for this block.
        Permutation permutation(n, blockStart);

        // Iterate over each permutation in the block.
        auto index = blockStart;
        while (1) 
        {
            const auto flips = permutation.countFlips();

            if (flips) 
            {
                if (index % 2 == 0)
                    checksum += flips;
                else
                    checksum -= flips;

                if (flips > maxFlips)
                    maxFlips = flips;
            }

            if (++index == blockStart + blockLength)
                break;

            // next permutation for this block.
            permutation.advance();
        }
    }

    // Output the results to stdout.
    cout << checksum << endl;
    cout << "Pfannkuchen(" << n << ") = " << maxFlips << endl;

    return 0;
}