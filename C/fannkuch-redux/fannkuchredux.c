// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Contributed by Jeremy Zerfas
// Based on the Ada program by Jonathan Parker and Georg Bauhaus which in turn
// was based on code by Dave Fladebo, Eckehard Berns, Heiner Marxen, Hongwei Xi,
// and The Anh Tran and also the Java program by Oleg Mazurov.

// This value controls how many blocks the workload is broken up into (as long
// as the value is less than or equal to the factorial of the argument to this
// program) in order to allow the blocks to be processed in parallel if
// possible. PREFERRED_NUMBER_OF_BLOCKS_TO_USE should be some number which
// divides evenly into all factorials larger than it. It should also be around
// 2-8 times the amount of threads you want to use in order to create enough
// blocks to more evenly distribute the workload amongst the threads.
#define PREFERRED_NUMBER_OF_BLOCKS_TO_USE 12

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

// intptr_t should be the native integer type on most sane systems.
typedef intptr_t intnative_t;


int main(int argc, char ** argv){
   const intnative_t n=atoi(argv[1]);

   // Create and initialize factorial_Lookup_Table.
   intnative_t factorial_Lookup_Table[n+1];
   factorial_Lookup_Table[0]=1;
   for(intnative_t i=0; ++i<=n;)
      factorial_Lookup_Table[i]=i*factorial_Lookup_Table[i-1];

   // Determine the block_Size to use. If n! is less than
   // PREFERRED_NUMBER_OF_BLOCKS_TO_USE then just use a single block to prevent
   // block_Size from being set to 0. This also causes smaller values of n to
   // be computed serially which is faster and uses less resources for small
   // values of n.
   const intnative_t block_Size=factorial_Lookup_Table[n]/
     (factorial_Lookup_Table[n]<PREFERRED_NUMBER_OF_BLOCKS_TO_USE ?
     1 : PREFERRED_NUMBER_OF_BLOCKS_TO_USE);

   intnative_t maximum_Flip_Count=0, checksum=0;

   // Iterate over each block.
   #pragma omp parallel for \
     reduction(max:maximum_Flip_Count) reduction(+:checksum)
   for(intnative_t initial_Permutation_Index_For_Block=0;
     initial_Permutation_Index_For_Block<factorial_Lookup_Table[n];
     initial_Permutation_Index_For_Block+=block_Size){

      intnative_t count[n];
      int8_t temp_Permutation[n], current_Permutation[n];


      // Initialize count and current_Permutation.
      count[0]=0;
      for(intnative_t i=0; i<n; ++i)
         current_Permutation[i]=i;
      for(intnative_t i=n-1,
        permutation_Index=initial_Permutation_Index_For_Block; i>0; --i){
         const intnative_t d=permutation_Index/factorial_Lookup_Table[i];
         permutation_Index=permutation_Index%factorial_Lookup_Table[i];
         count[i]=d;

         for(intnative_t j=0; j<n; ++j)
            temp_Permutation[j]=current_Permutation[j];
         for(intnative_t j=0; j<=i; ++j)
            current_Permutation[j]= j+d<=i ?
              temp_Permutation[j+d] : temp_Permutation[j+d-i-1];
      }


      // Iterate over each permutation in the block.
      const intnative_t last_Permutation_Index_In_Block=
        initial_Permutation_Index_For_Block+block_Size-1;
      for(intnative_t permutation_Index=initial_Permutation_Index_For_Block; ;
        ++permutation_Index){

         // If the first value in the current_Permutation is not 1 (0) then
         // we will need to do at least one flip for the current_Permutation.
         if(current_Permutation[0]>0){

            // Make a copy of current_Permutation[] to work on. Note that we
            // don't need to copy the first value since that will be stored
            // in a separate variable since it gets used a lot.
            for(intnative_t i=0; ++i<n;)
               temp_Permutation[i]=current_Permutation[i];

            intnative_t flip_Count=1;

            // Flip temp_Permutation until the element at the first_Value
            // index is 1 (0).
            for(intnative_t first_Value=current_Permutation[0];
              temp_Permutation[first_Value]>0; ++flip_Count){

               // Record the new_First_Value and restore the old
               // first_Value at its new flipped position.
               const int8_t new_First_Value=temp_Permutation[first_Value];
               temp_Permutation[first_Value]=first_Value;

               // If first_Value is greater than 3 (2) then we are flipping
               // a series of four or more values so we will also need to
               // flip additional elements in the middle of the
               // temp_Permutation.
               if(first_Value>2){
                  intnative_t low_Index=1, high_Index=first_Value-1;
                  // Note that this loop is written so that it will run at
                  // most 16 times so that compilers will be more willing
                  // to unroll it. Consequently this won't work right when
                  // n is greater than 35. This would probably be the
                  // least of your concerns since 21! won't fit into 64
                  // bit integers and even if it did you probably wouldn't
                  // want to run this program with a value that large
                  // since it would take thousands of years to do on a
                  // modern desktop computer. ;-)
                  do{
                     const int8_t temp=temp_Permutation[high_Index];
                     temp_Permutation[high_Index]=
                       temp_Permutation[low_Index];
                     temp_Permutation[low_Index]=temp;
                  }while(low_Index+++3<=high_Index-- && low_Index<16);
               }

               // Update first_Value to new_First_Value that we recorded
               // earlier.
               first_Value=new_First_Value;
            }


            // Update the checksum.
            if(permutation_Index%2==0)
               checksum+=flip_Count;
            else
               checksum-=flip_Count;

            // Update maximum_Flip_Count if necessary.
            if(flip_Count>maximum_Flip_Count)
               maximum_Flip_Count=flip_Count;
         }


         // Break out of the loop when we get to the
         // last_Permutation_Index_In_Block.
         if(permutation_Index>=last_Permutation_Index_In_Block)
            break;

         // Generate the next permutation.
         int8_t first_Value=current_Permutation[1];
         current_Permutation[1]=current_Permutation[0];
         current_Permutation[0]=first_Value;
         for(intnative_t i=1; ++count[i]>i;){
            count[i++]=0;
            const int8_t new_First_Value=current_Permutation[0]=
              current_Permutation[1];

            for(intnative_t j=0; ++j<i;)
               current_Permutation[j]=current_Permutation[j+1];

            current_Permutation[i]=first_Value;
            first_Value=new_First_Value;
         }
      }
   }


   // Output the results to stdout.
   printf("%jd\nPfannkuchen(%jd) = %jd\n", (intmax_t)checksum, (intmax_t)n,
     (intmax_t)maximum_Flip_Count);

   return 0;
}
    