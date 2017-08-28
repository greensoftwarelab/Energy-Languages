/* The Computer Language Benchmarks Game
http://benchmarksgame.alioth.debian.org/

Contributed by Andrew Moon
*/

#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>
#include <string.h>

struct CPUs {
   CPUs() {
      cpu_set_t cs;
      CPU_ZERO( &cs );
      sched_getaffinity( 0, sizeof(cs), &cs );
      count = 0;
      for ( size_t i = 0; i < CPU_SETSIZE; i++ )
         count += CPU_ISSET( i, &cs ) ? 1 : 0;
      count = std::max( count, size_t(1) );
   }

   size_t count;
} cpus;

struct ReverseLookup {
   ReverseLookup( const char *from, const char *to ) {
      for ( int i = 0; i < 256; i++ )
         byteLookup[i] = i;
      for ( ; *from && *to; from++, to++ ) {
         byteLookup[toupper(*from)] = *to;
         byteLookup[tolower(*from)] = *to;
      }

      for ( int i = 0; i < 256; i++ )
         for ( int j = 0; j < 256; j++ )
            wordLookup[(i << 8) | j] = ( byteLookup[j] << 8 ) | byteLookup[i];
   }

   char operator[]( const char &c ) { return (char )byteLookup[(unsigned char )c]; }
   short operator[]( const short &s ) { return (short )wordLookup[(unsigned short )s]; }

protected:
   unsigned char byteLookup[256];
   unsigned short wordLookup[256*256];
} lookup( "acbdghkmnsrutwvy", "TGVHCDMKNSYAAWBR" );

template< class type >
struct vector2 : public std::vector<type> {
   type &last() { return this->operator[]( std::vector<type>::size() -1 ); }
};

struct Chunker {
   enum { lineLength = 60, chunkSize = 65536, };

   Chunker( int seq ) : id(seq) {}

   struct Chunk {
      Chunk() {}
      Chunk( char *in, size_t amt ) : data(in), size(amt) {}
      char *data;
      size_t size;
   };

   void NewChunk() {
      size_t cur = mark - chunkBase;
      chunks.push_back( Chunk( chunkBase, cur ) );
      chunkBase += ( cur + ( cur & 1 ) ); // keep it word aligned
      mark = chunkBase;
   }

   template< int N >
   struct LinePrinter {
      LinePrinter() : lineFill(0) {}
      void endofblock() { if ( lineFill ) newline(); }
      void emit( const char *str, size_t amt ) { 
         fwrite_unlocked( str, 1, amt, stdout );
      }
      void emit( char c ) { fputc_unlocked( c, stdout ); }
      void emitnewline() { emit( '\n' ); }
      void emitlines( char *data, size_t size ) {
         if ( lineFill ) {
            size_t toprint = std::min( size, lineLength - lineFill );
            emit( data, toprint );
            size -= toprint;
            data += toprint;
            lineFill += toprint;
            if ( lineFill == lineLength )
               newline();
         }

         while ( size >= lineLength ) {
            emit( data, lineLength );
            emitnewline();
            size -= lineLength;
            data += lineLength;
         }

         if ( size ) {
            lineFill = size;
            emit( data, size );
         }
      }
      void newline() { lineFill = 0; emitnewline(); }
      void reset() { lineFill = 0; }
   protected:
      size_t lineFill;
   };

   void Print() {
      int prevId = -( id - 1 );
      while ( __sync_val_compare_and_swap( &printQueue, prevId, id ) != prevId )
         sched_yield();

      fwrite_unlocked( name, 1, strlen( name ), stdout );
      static LinePrinter<65536*2> line;
      line.reset();
      for ( int i = int(chunks.size()) - 1; i >= 0; i-- )
         line.emitlines( chunks[i].data, chunks[i].size );
      line.endofblock();

      __sync_val_compare_and_swap( &printQueue, id, -id );
   }

   // fseek on stdin seems flaky so this hack. not called often
   void Backup() {
      while ( true ) {
         if ( fgetc_unlocked( stdin ) == '>' ) {
            fseek( stdin, -1, SEEK_CUR );
            return;
         }
         fseek( stdin, -2, SEEK_CUR );
      }
   }

   // input buffer can hold all of stdin, so no size checking
   size_t Read( char *data ) {
      if ( feof( stdin ) )
         return 0;

      name = data;
      fgets_unlocked( name, 128, stdin );
      mark = chunkBase = name + strlen( name ) + 1;
      mark[lineLength] = -1;

      while ( fgets_unlocked( mark, 128, stdin ) ) {
         if ( *mark == '>' ) {
            Backup();
            break;
         }

         // mark trick should keep us from calling strlen
         mark += ( mark[lineLength] != 0xa ) ? strlen( mark ) - 1 : lineLength;
         if ( mark - chunkBase > chunkSize )
            NewChunk();

         mark[lineLength] = -1;
      }

      if ( mark - chunkBase )
         NewChunk();
      return ( chunkBase - data );
   }

   struct WorkerState {
      Chunker *chunker;
      size_t offset, count;
      pthread_t handle;
   };

   static void *ReverseWorker( void *arg ) {
      WorkerState *state = (WorkerState *)arg;
      Chunker &chunker = *state->chunker;
      for ( size_t i = 0; i < state->count; i++ ) {
         Chunk &chunk = chunker[state->offset + i];
         short *w = (short *)chunk.data, *bot = w, *top = w + ( chunk.size / 2 ) - 1;
         for ( ; bot < top; bot++, top-- ) {
            short tmp = lookup[*bot];
            *bot = lookup[*top];
            *top = tmp;
         }
         // if size is odd, final byte would reverse to the start (skip it)
         if ( chunk.size & 1 )
            chunk.data++;
      }
      return 0;
   }

   void Reverse() {
      if ( !chunks.size() )
         return;

      // this takes so little time it's almost not worth parallelizing
      vector2<WorkerState> threads;
      threads.reserve( cpus.count );
      size_t divs = chunks.size() / cpus.count;
      for ( size_t i = 0, offset = 0; i < cpus.count; i++, offset += divs ) {
         threads.push_back( WorkerState() );
         WorkerState &ws = threads.last();
         ws.chunker = this;
         ws.count = ( i < cpus.count - 1 ) ? divs : chunks.size() - offset;
         ws.offset = offset;
         pthread_create( &ws.handle, 0, ReverseWorker, &ws );
      }

      for ( size_t i = 0; i < cpus.count; i++ )
         pthread_join( threads[i].handle, 0 );
   }

   Chunk &operator[] ( size_t i ) { return chunks[i]; }

protected:
   vector2<Chunk> chunks;
   char *name, *chunkBase, *mark;
   int id;
   static volatile int printQueue;
};

// used to order chunk printing
volatile int Chunker::printQueue = 0;

struct ReverseComplement {
   ReverseComplement() {
      // get stdin file size
      long start = ftell( stdin );
      fseek( stdin, 0, SEEK_END );
      size = ftell( stdin ) - start;
      fseek( stdin, start, SEEK_SET );

      data = new char[size + 3];
   }

   ~ReverseComplement() {
      delete[] data;
   }

   static void *ChunkerThread( void *arg ) {
      Chunker *chunker = (Chunker *)arg;
      chunker->Reverse();
      chunker->Print();
      return 0;
   }

   void Run() {
      vector2<Chunker *> chunkers;
      vector2<pthread_t> threads;

      size_t cur = 0;
      for ( int id = 1; true; id++ ) {
         chunkers.push_back( new Chunker( id ) );

         size_t read = chunkers.last()->Read( data + cur );
         cur += read;
         if ( !read )
            break;

         // spawn off a thread to finish this guy up while we read another chunk in
         threads.push_back( 0 );
         pthread_create( &threads.last(), 0, ChunkerThread, chunkers.last() );
      }

      for ( size_t i = 0; i < threads.size(); i++ )
         pthread_join( threads[i], 0 );

      for ( size_t i = 0; i < chunkers.size(); i++ )
         delete chunkers[i];
   }


protected:
   size_t size;
   char *data;
};


int main( int argc, const char *argv[] ) {
   ReverseComplement revcom;
   revcom.Run();
   return 0;
}