// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Contributed by Kevin Miller ( as C code )
//
// Ported to C++ with minor changes by Dave Compton
//
// compile with following g++ flags
//  -pipe -Wall -O3  -fomit-frame-pointer -march=native -mfpmath=sse -msse2 -fopenmp   -mno-fma --std=c++14 

#include <immintrin.h>
#include <iostream>

#ifdef __AVX__
#define VEC_SIZE 4
typedef __m256d Vec;
#define VEC_INIT(value) (Vec){value,value,value,value}
#else
#define VEC_SIZE 2
typedef __m128d Vec;
#define VEC_INIT(value) (Vec){value,value}
#endif

#define LOOP_SIZE (8/VEC_SIZE)

using namespace std;

int numDigits(long n)
{
    auto len = 0;
    while(n)
    {
        n=n/10;
        len++;
    }
    return len;
}

// Return true iff any of 8 members of double vector v is 
// less than or equal to f.
bool vec_le(double *v, double f)
{
    return (
        v[0] <= f ||
        v[1] <= f ||
        v[2] <= f ||
        v[3] <= f ||
        v[4] <= f ||
        v[5] <= f ||
        v[6] <= f ||
        v[7] <= f);
}

// Return 8 bit value with bits set iff cooresponding 
// member of double vector v is less than or equal to f.
int8_t pixels(double *v, double f)
{
    int8_t res = 0;
    if(v[0] <= f) res |= 0b10000000;
    if(v[1] <= f) res |= 0b01000000;
    if(v[2] <= f) res |= 0b00100000;
    if(v[3] <= f) res |= 0b00010000;
    if(v[4] <= f) res |= 0b00001000;
    if(v[5] <= f) res |= 0b00000100;
    if(v[6] <= f) res |= 0b00000010;
    if(v[7] <= f) res |= 0b00000001;
    return res;
}

//
// Do one iteration of mandelbrot calculation for a vector of eight 
// complex values.  Using Vec to work with groups of doubles speeds
// up computations.
//
inline void calcSum(double *r, double *i, double *sum, double const *init_r, Vec const &init_i)
{
    auto r_v = (Vec*)r;
    auto i_v = (Vec*)i;
    auto sum_v = (Vec*)sum;
    auto init_r_v = (Vec const *)init_r;

    for(auto vec=0; vec<LOOP_SIZE; vec++)
    {
        auto r2 = r_v[vec] * r_v[vec];
        auto i2 = i_v[vec] * i_v[vec];
        auto ri = r_v[vec] * i_v[vec];

        sum_v[vec] = r2 + i2;

        r_v[vec]=r2 - i2 + init_r_v[vec];
        i_v[vec]=ri + ri + init_i;
    }
}

//
// Do 50 iterations of mandelbrot calculation for a vector of eight 
// complex values.  Check occasionally to see if the iterated results
// have wandered beyond the point of no return (> 4.0).
//
inline int8_t mand8(double *init_r, double iy)
{
    double r[8], i[8], sum[8];
    for(auto k=0; k<8; k++)
    {
        r[k]=init_r[k];
        i[k]=iy;
    }

    auto init_i = VEC_INIT(iy);

    int8_t pix = 0xff;

    for (auto j = 0; j < 10; j++)
    {
        for(auto k=0; k<5; k++)
            calcSum(r, i, sum, init_r, init_i);

        if (!vec_le(sum, 4.0))
        {
            pix = 0x00;
            break;
        }
    }
    if (pix)
    {
        pix = pixels(sum, 4.0);
    }

    return pix;
}

int main(int argc, char ** argv)
{
    // get width/height from arguments

    auto wid_ht = 16000;
    if (argc >= 2)
    {
        wid_ht = atoi(argv[1]);
    }

    // round up to multiple of 8
    wid_ht = (wid_ht+7) & ~7;  

    // allocate memory for pixels.
    auto dataLength = wid_ht*(wid_ht>>3);
    auto pixels = new char[dataLength];

    // calculate initial x values, store in r0
    double r0[wid_ht];
    for(auto x=0; x<wid_ht; x++)
    {
        r0[x] = 2.0 / wid_ht * x - 1.5;
    }

    // generate the bitmap

    // process 8 pixels (one byte) at a time    
    #pragma omp parallel for schedule(guided)
    for(auto y=0; y<wid_ht; y++)
    {
        // all 8 pixels have same y value (iy).
        auto iy = 2.0 / wid_ht *  y - 1.0;
        auto rowstart = y*wid_ht/8;
        for(auto x=0; x<wid_ht; x+=8)
        {
            pixels[rowstart + x/8] = mand8(r0+x,iy); 
        }
    }

    // write the data
    cout << "P4\n" << wid_ht << " " << wid_ht << "\n";
    cout.write(pixels, dataLength);
    delete[] pixels;

    return 0;
}