/* The Computer Language Benchmarks Game
 http://benchmarksgame.alioth.debian.org/
 contributed by Ralph Ganszky
 converted to Swift 3 by Daniel Muellenborn
 */

import Glibc
import Dispatch

// Get matrix dimension
let n: Int
if CommandLine.arguments.count > 1 {
   n = Int(CommandLine.arguments[1]) ?? 5500
} else {
   n = 5500
}

let queue = DispatchQueue.global(qos: .default)

func A(_ i: Int, _ j: Int) -> Double {
   return 1.0 / Double((i+j)*(i+j+1)/2 + (i+1))
}

func multiplyAv(_ n: Int, _ v: [Double]) -> [Double] {
   var Av = [Double](repeating: 0.0, count: n)
   DispatchQueue.concurrentPerform(iterations: n) { i in
      var AvTemp = 0.0
      for j in 0..<n {
         AvTemp += A(i,j) * v[j]
      }
      Av[i] = AvTemp      // This is unprotected and could cause
      // a problem. Maybe here some atomic update
      // or a semaphore should be used
   }
   return Av
}

func multiplyAtv(_ n: Int, _ v: [Double]) -> [Double] {
   var Atv = [Double](repeating: 0.0, count: n)
   DispatchQueue.concurrentPerform(iterations: n) { i in
      var AtvTemp = 0.0
      for j in 0..<n {
         AtvTemp += A(j,i) * v[j]
      }
      Atv[i] = AtvTemp
   }
   return Atv
}

func multiplyAtAv(_ n: Int, _ v: [Double]) -> [Double] {
   let u = multiplyAv(n, v)
   return multiplyAtv(n, u)
}

func approximate(_ n: Int) -> Double {
   var u = [Double](repeating: 1.0, count: n)
   var v = [Double]()
   for _ in 0..<10 {
      v = multiplyAtAv(n, u)
      u = multiplyAtAv(n, v)
   }
   var vBv = 0.0
   var vv = 0.0
   for i in 0..<n {
      vBv += u[i]*v[i]
      vv += v[i]*v[i]
   }
   return sqrt(vBv/vv)
}

func roundDouble(_ num: Double, precision: Int) -> String {
   let exponent = pow(10.0, Double(precision))
   let number = Double(Int(num * exponent + 0.5))/exponent
   return "\(number)"
}

print(roundDouble(approximate(n), precision: 9))