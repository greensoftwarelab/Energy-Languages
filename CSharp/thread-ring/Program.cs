/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
 * contributed by Isaac Gouy
 */

using System;
using System.Threading;


internal class NamedThread
{
   private int name;
   private AutoResetEvent signal = new AutoResetEvent(false);
   private int token = 0;

   internal NamedThread(int name) {
      this.name = name;
   }

   internal void Run() {
      while (TokenNotDone()) 
         NextThread().TakeToken(token-1);

      if (token == 0) Console.WriteLine(name);
      NextThread().TakeToken(-1);
   }

   private bool TokenNotDone() {
      signal.WaitOne();
      return token > 0;
   }

   internal NamedThread NextThread() {
      return ThreadRing.threadRing[ name % ThreadRing.numberOfThreads ];
   }

   internal void TakeToken(int x) {
      token = x;
      signal.Set();
   }
}


public class ThreadRing
{
   internal const int numberOfThreads = 503;
   internal static NamedThread[] threadRing = new NamedThread[503];

   public static void Main(string[] args) {
      for (int i = 0; i < numberOfThreads; i++){ 
         threadRing[i] = new NamedThread(i+1);
      }

      foreach (NamedThread t in threadRing) 
         new Thread(new ThreadStart(t.Run)).Start(); 

      threadRing[0].TakeToken( int.Parse(args[0]) );
   }
}