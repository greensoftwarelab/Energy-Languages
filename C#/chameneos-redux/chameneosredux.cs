/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   Adapted by Pascal Fresnay from Java version that was:
      contributed by Michael Barker
      based on a contribution by Luzius Meisser
*/

/**
 * This implementation uses standard C# threading (native threads).
 *
 * This implementation simply adds the new functionality to the orginal
 * implementation by Luzius Meisser from old chameneos task.  The interesting
 * part of this implementation, is that while a creature is waiting it does not
 * block its thread, rather it spins in a loop using a Thread.Sleep(0).
 */


using System;
using System.Threading;
using System.Text;
public class chameneosredux {

   public enum Colour {
      blue,
      red,
      yellow
   }

   private static Colour doCompliment(Colour c1, Colour c2) {
      switch (c1) {
      case Colour.blue:
         switch (c2) {
         case Colour.blue:
            return Colour.blue;
         case Colour.red:
            return Colour.yellow;
         case Colour.yellow:
            return Colour.red;
         default: break;
         }
         break;
      case Colour.red:
         switch (c2) {
         case Colour.blue:
            return Colour.yellow;
         case Colour.red:
            return Colour.red;
         case Colour.yellow:
            return Colour.blue;
         default: break;
         }
         break;
      case Colour.yellow:
         switch (c2) {
         case Colour.blue:
            return Colour.red;
         case Colour.red:
            return Colour.blue;
         case Colour.yellow:
            return Colour.yellow;
         default: break;
         }
         break;
      default: break;
      }
      throw new Exception();
   }

   sealed class MeetingPlace {

      private int meetingsLeft;

      public MeetingPlace(int meetings) {
         this.meetingsLeft = meetings;
      }

      private Colour? firstColour = null;
      private int firstId = 0;
      Future current;

      public Pair meet(int id, Colour c){
         Future newPair;
         lock (this) {
            if (meetingsLeft == 0) {
               throw new Exception("Finished");
            } else {
               if (firstColour == null) {
                  firstColour = c;
                  firstId = id;
                  current = new Future();
               } else {
                  Colour newColour = doCompliment(c, firstColour.Value);
                  current.Item = new Pair(id == firstId, newColour);
                  firstColour = null;
                  meetingsLeft--;
               }
               newPair = current;
            }
         }
         return newPair.Item;
      }
   }

   public sealed class Future {

      private Pair? t;
      
      public Pair Item
      {
         get
         {
            while (t == null)
            {
               Thread.Sleep(0);
            }
            return t.Value;
         }
         // need synchronization ?
         set
         {
            t = value;
         }
      }
   }

   sealed class Creature{

      private readonly MeetingPlace place;
      private int count = 0;
      private int sameCount = 0;
      private Colour colour;
      private int id;

      public Creature(MeetingPlace place, Colour colour) {
         this.place = place;
         this.id = this.GetHashCode();
         this.colour = colour;
      }

      public void run() {
         try {

            while (true) {
               Pair p = place.meet(id, colour);
               colour = p.colour;
               if (p.sameId) {
                  sameCount++;
               }
               count++;
            }

         } catch (Exception) {}
      }

      public int Count {
         get
         {
            return count;
         }
      }

      public override String ToString() {
         return count.ToString() + getNumber(sameCount);
      }
   }

   private static void run(int n, params Colour[] colours) {
      MeetingPlace place = new MeetingPlace(n);
      Creature[] creatures = new Creature[colours.Length];
      for (int i = 0; i < colours.Length; i++) {
         Console.Write(" " + colours[i]);
         creatures[i] = new Creature(place, colours[i]);
      }
      Console.WriteLine();
      Thread[] ts = new Thread[colours.Length];
      for (int i = 0; i < colours.Length; i++) {
         ts[i] = new Thread(creatures[i].run);
         ts[i].Start();
      }

      foreach (Thread t in ts) {
            t.Join();
      }

      int total = 0;
      foreach (Creature creature in creatures) {
         Console.WriteLine(creature);
         total += creature.Count;
      }
      Console.WriteLine(getNumber(total));
      Console.WriteLine();
   }

   public static void Main(String[] args) {

      int n = 600;
      if(args.Length > 0)
         n = Int32.Parse(args[0]);

      printColours();
      Console.WriteLine();
      run(n, Colour.blue, Colour.red, Colour.yellow);
      run(n, Colour.blue, Colour.red, Colour.yellow, Colour.red, Colour.yellow,
            Colour.blue, Colour.red, Colour.yellow, Colour.red, Colour.blue);
   }

   public struct Pair {
      public readonly bool sameId;
      public readonly Colour colour;

      public Pair(bool sameId, Colour c) {
         this.sameId = sameId;
         this.colour = c;
      }
   }

   private static String[] NUMBERS = {
      "zero", "one", "two", "three", "four", "five",
      "six", "seven", "eight", "nine"
   };

   private static String getNumber(int n) {
      StringBuilder sb = new StringBuilder();
      String nStr = n.ToString();
      for (int i = 0; i < nStr.Length; i++) {
         sb.Append(" ");
         sb.Append(NUMBERS[(int)Char.GetNumericValue(nStr[i])]);
      }

      return sb.ToString();
   }

   private static void printColours() {
      printColours(Colour.blue, Colour.blue);
      printColours(Colour.blue, Colour.red);
      printColours(Colour.blue, Colour.yellow);
      printColours(Colour.red, Colour.blue);
      printColours(Colour.red, Colour.red);
      printColours(Colour.red, Colour.yellow);
      printColours(Colour.yellow, Colour.blue);
      printColours(Colour.yellow, Colour.red);
      printColours(Colour.yellow, Colour.yellow);
   }

   private static void printColours(Colour c1, Colour c2) {
      Console.WriteLine(c1 + " + " + c2 + " -> " + doCompliment(c1, c2));
   }


}