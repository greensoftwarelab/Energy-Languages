// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Contributed by Jeremy Zerfas

// This string/character array is used to convert characters into the
// complementing character.
#define COMPLEMENT_LOOKUP \
  "                                                                "\
  /*ABCDEFGHIJKLMNOPQRSTUVWXYZ      abcdefghijklmnopqrstuvwxyz    */\
  " TVGH  CD  M KN   YSAABW R       TVGH  CD  M KN   YSAABW R"

// This controls the size of reads from the input and is also used as the
// initial sequence_Capacity.
#define READ_SIZE 16384

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// intptr_t should be the native integer type on most sane systems.
typedef intptr_t intnative_t;


static intnative_t next_Sequence_Number_To_Output=1;

static void process_Sequence(char * sequence, const intnative_t sequence_Size
  , const intnative_t sequence_Number){
   // Free up any memory we didn't need.
   sequence=realloc(sequence, sequence_Size);

   // Set up pointers to the front_Pos and bac_Pos, advance front_Pos to the
   // first character on the next line, and then make sure front_Pos and
   // back_Pos start out pointing to non-line feed characters (unless all the
   // characters happen to be line feeds in which case front_Pos will go past
   // back_Pos causing the reversing and complementing loop to do nothing.
   char * front_Pos=sequence, * back_Pos=sequence+sequence_Size-1;
   while(*front_Pos++!='\n');
   while(*front_Pos=='\n' && front_Pos<=back_Pos) front_Pos++;
   while(*back_Pos=='\n' && front_Pos<=back_Pos) back_Pos--;

   // Reverse and complement the sequence.
   while(front_Pos<=back_Pos){
      const char temp=COMPLEMENT_LOOKUP[(unsigned char)*front_Pos];
      *front_Pos=COMPLEMENT_LOOKUP[(unsigned char)*back_Pos];
      *back_Pos=temp;

      // Skip over line feeds.
      while(*++front_Pos=='\n');
      while(*--back_Pos=='\n');
   }

   // Wait for our turn to output the altered sequence and then output it.
   #pragma omp flush(next_Sequence_Number_To_Output)
   while(sequence_Number!=next_Sequence_Number_To_Output){
      #pragma omp flush(next_Sequence_Number_To_Output)
   }
   fwrite(sequence, 1, sequence_Size, stdout);
   next_Sequence_Number_To_Output++;
   #pragma omp flush(next_Sequence_Number_To_Output)

   // Free the memory for the altered sequence.
   free(sequence);
}


int main(){
   #pragma omp parallel
   {
      #pragma omp single
      {
         // Allocate memory for the initial sequence (assuming there is one).
         intnative_t sequence_Capacity=READ_SIZE, sequence_Size=0
           , sequence_Number=1;
         char * sequence=malloc(sequence_Capacity);

         // Read in sequence data until we reach the end of the file or
         // encounter an error.
         for(intnative_t bytes_Read; (bytes_Read
           =fread(&sequence[sequence_Size], 1, READ_SIZE, stdin)); ){


            // Search the read in chunk of data for a '>' to see if any
            // sequences are being started.
            for(char * sequence_Start; (sequence_Start
              =memchr(&sequence[sequence_Size], '>', bytes_Read)); ){

               // Update the sequence_Size to reflect any data before the
               // '>' that was read in.
               const intnative_t number_Of_Preceding_Bytes
                 =sequence_Start-&sequence[sequence_Size];
               sequence_Size+=number_Of_Preceding_Bytes;


               // If there is any data for the current sequence, then
               // start processing it.
               if(sequence_Size){

                  // Allocate memory for a new sequence and copy the '>'
                  // & any data following it to the new sequence.
                  char * const new_Sequence=malloc(READ_SIZE);
                  memcpy(new_Sequence, sequence_Start
                    , bytes_Read-number_Of_Preceding_Bytes);

                  // Process the current sequence and have another thread
                  // do the work if OpenMP is enabled and there is more
                  // than one CPU core.
                  #pragma omp task\
                    firstprivate(sequence, sequence_Size, sequence_Number)
                  {
                     process_Sequence(sequence, sequence_Size
                       , sequence_Number);
                  }

                  // Update variables to reflect the new sequence.
                  sequence=new_Sequence;
                  sequence_Capacity=READ_SIZE;
                  sequence_Size=0;
                  sequence_Number++;
               }


               // Update sequence_Size and bytes_Read to reflect the read
               // in '>' and any data that preceded it.
               sequence_Size++;
               bytes_Read-=number_Of_Preceding_Bytes+1;
            }


            // Update sequence_Size to reflect the bytes that were read in.
            sequence_Size+=bytes_Read;

            // If there potentially isn't enough free space for all the data
            // from the next read, then double the capacity of the sequence.
            if(sequence_Size>sequence_Capacity-READ_SIZE)
               sequence=realloc(sequence, sequence_Capacity*=2);
         }


         // If there is any data for a last sequence, process it, otherwise
         // just free the sequence memory.
         if(sequence_Size)
            process_Sequence(sequence, sequence_Size, sequence_Number);
         else
            free(sequence);
      }
   }

   return 0;
}