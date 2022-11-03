/* The Computer Language Benchmarks Game
http://benchmarksgame.alioth.debian.org/

Based on C contribution by Alex Burlyga
Based on Java contribution by Michael Barker
Based on the original C++ contribution by The Anh Tran 
Based on the #5 C contribution by Dmitry Vyukov 
Contributed & Modified by Andrew Moon

Each chameneous creature is a standard OS thread.
Data exchange mechanism is gcc built-in atomic ops.
*/


#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <string>

using namespace std;

struct CPUs {
   enum { perslot = 2 };
   CPUs() {
      for ( int i = 0; i < 33; i++ )
         CPU_ZERO( &affinities[i] );
      cpu_set_t &cs = affinities[0];
      sched_getaffinity( 0, sizeof(cs), &cs );

      count = 0;
      for ( int i = 0; i < CPU_SETSIZE; i++ ) {
         if ( CPU_ISSET( i, &cs ) ) {
            CPU_SET( i, &affinities[(count / perslot) + 1] );
            count++;
         }
      }
      mod = ( count > 2 ) ? count >> 1 : 1;
   }
   
   cpu_set_t *getaffinity( int slot ) { 
      return &affinities[ slot ? ( slot % mod ) + 1 : 0 ]; 
   }

   int count, mod;
   cpu_set_t affinities[33]; // up to 64 cores!
} cpus;

// kludge to make running on a single core at least SOMEWHAT performant
struct SingleCoreYield {
   SingleCoreYield() : counter(0) {}
   void Run() {
      if ( cpus.count <= 1 || counter++ > 20000 ) {
         sched_yield();
         counter = 0;
      }
   }

protected:
   int counter;
};

enum Color { blue = 0, red, yellow, Invalid };

// stream operator to write a color
ostream &operator<< ( ostream &s, const Color &c ) {
   static const char *names[] = { "blue", "red", "yellow", "Invalid" };
   s << names[c];
   return s;
}

// +operator to add colors
Color operator+( const Color &c1, const Color &c2 ) {
   switch ( c1 ) {
      case blue: switch ( c2 ) {
         case blue:   return blue;
         case red:    return yellow;
         case yellow: return red;
         default:;
      }
      case red: switch ( c2 ) {
         case blue:   return yellow;
         case red:    return red;
         case yellow: return blue;
         default:;
      }
      case yellow: switch ( c2 ) {
         case blue:   return red;
         case red:    return blue;
         case yellow: return yellow;
         default:;
      }
      default:;
   }
   return Invalid;
}


// spells out a number as named digits
string SpellNumber( int n ) {
   static const char *numbers[] = {
      " zero", " one", " two",
      " three", " four", " five",
      " six", " seven", " eight",
      " nine"
   };

   string str;
   do {
      str.insert( 0, numbers[n % 10] );
      n /= 10;
   } while ( n );

   return str;
}

struct MeetingPlace;

struct Creature {
   Creature() : id(0), count(0), sameCount(0), met(false) {}

   // output our total visits and self visits, returning total visits
   int Display() const {
      cout << count << SpellNumber(sameCount) << endl;
      return count;
   }

   void Meet( Creature *other ) {
      if ( id == other->id ) {
         sameCount++;
         other->sameCount++;
      }

      count++;
      other->count++;

      Color newcolor = color + other->color;
      other->color = color = newcolor;
      other->met = true;
   }

   void Init( MeetingPlace *mp, Color c );
   void Run();

   void Start( int affinity = 0 ) {
      pthread_attr_init( &threadAttr );
      if ( cpus.count >= 4 ) {
         cpu_set_t *cores = cpus.getaffinity( affinity );
         pthread_attr_setaffinity_np( &threadAttr, sizeof(cpu_set_t), cores );
      }
      pthread_create( &threadHandle, &threadAttr, &Creature::ThreadRun, this );
   }

   static void *ThreadRun( void *param ) {
      ((Creature*)param)->Run();
      return 0;
   }

   void Wait() const {
      pthread_join( threadHandle, NULL );
   }

   void WaitUntilMet() {
      SingleCoreYield yield;
      while ( !met )
         yield.Run();
      met = false;
   }

   int id, count, sameCount;
   volatile bool met; // met is set from other threads, don't cache in a register
   Color initialColor, color;

protected:
   pthread_t threadHandle;
   pthread_attr_t threadAttr;
   MeetingPlace *place;
};

struct MeetingPlace {
   // max # of creatures is ( 1 << S ) - 1, max # of meetings is ( 1 << ( 32 - S ) ) - 1
   enum { S = 4, creatureMask = (1 << S) - 1 };
   MeetingPlace( int N ) : state(N << S), idGenerator(1) { creatures = new Creature *[N]; }
   ~MeetingPlace() { delete[] creatures; }
   
   void Register( Creature &creature ) {
      creature.id = idGenerator++;
      creatures[creature.id] = &creature;
   }

   void MeetUp( Creature *creature ) {
      int useState = state;
      while ( true ) {
         int waiting = useState & creatureMask, tryState = 0;
         if ( waiting )
            // there's a waiting creature, set the new state to meetingCount - 1
            tryState = ( ( useState & ~creatureMask ) - ( 1 << S ) );
         else if ( useState )
            // nobody waiting and meetings left, set ourselves to the waiting creature
            tryState = useState | creature->id;
         else
            // nobody waiting and no meetings left, we're done
            return;

         int oldState = __sync_val_compare_and_swap( &state, useState, tryState );
         if ( oldState == useState ) {
            if ( waiting )
               creature->Meet( creatures[waiting] );
            else
               creature->WaitUntilMet();
            useState = state;
         } else {
            useState = oldState;
         }
      }
   }

protected:
   volatile int state; // state is read & set from other threads, don't cache in a register
   int idGenerator;
   Creature **creatures;
};



void Creature::Init( MeetingPlace *mp, Color c ) {
   place = mp;
   initialColor = color = c;
   place->Register( *this );
}

void Creature::Run() {
   place->MeetUp( this );
}


template< int ncolor >
struct Game {
   Game( int meetings, const Color (&color)[ncolor] ) : meetingPlace(meetings) {
      for ( int i = 0; i < ncolor; i++ )
         creatures[i].Init( &meetingPlace, color[i] );
   }
   
   void Start(  int affinity = 0 ) {
      for ( int i = 0; i < ncolor; i++ )
         creatures[i].Start( affinity );
   }

   void Wait() {
      for ( int i = 0; i < ncolor; i++ )
         creatures[i].Wait();
   }

   void Display() {
      // display the initial color list
      for ( int i = 0; i < ncolor; i++ )
         cout << " " << creatures[i].initialColor;
      cout << endl;

      // output each creature and sum up the total visits
      int total = 0;
      for ( int i = 0; i < ncolor; i++ )
         total += creatures[i].Display();
      cout << SpellNumber(total) << endl << endl;
   }

protected:
   MeetingPlace meetingPlace;
   Creature creatures[ncolor];
};


int main( int argc, const char *argv[] ) {
   const Color r1[] = {
      blue, red, yellow
   };

   const Color r2[] = {
      blue, red, yellow,
      red, yellow, blue,
      red, yellow, red,
      blue
   };

   for ( int c1 = blue; c1 <= yellow; c1++ )
      for ( int c2 = blue; c2 <= yellow; c2++ )
         cout << r1[c1] << " + " << r1[c2] << " -> " << ( r1[c1] + r1[c2] ) << endl;
   cout << endl;

   int n = ( argc >= 2 ) ? atoi( argv[1] ) : 6000000;

   Game< 3> g1( n, r1 ); 
   Game<10> g2( n, r2 );
   if ( cpus.count < 4 ) {
      g1.Start(); g1.Wait();
      g2.Start(); g2.Wait();
   } else {
      g1.Start(1); g2.Start(2);
      g1.Wait(); g2.Wait();
   }
   g1.Display();
   g2.Display();
}