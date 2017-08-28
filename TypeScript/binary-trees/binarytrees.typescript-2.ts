/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Isaac Gouy 
   *reset*
*/


/// <reference path="../node_modules/@types/node/index.d.ts" />

class TreeNode {
   constructor(
      private left: TreeNode, 
      private right: TreeNode, 
   ) { }

   check(): number {
      if (this.left) {
         return 1 + this.left.check() + this.right.check()
      }
      else {
         return 1
      }
   }
}

function bottomUpTree(depth: number): TreeNode {
   if (depth > 0) {
      // "new TreeNode(" must be on same line as "return" 
      return new TreeNode(
         bottomUpTree(depth-1),
         bottomUpTree(depth-1)
      )
   }
   else {
      return new TreeNode(undefined,undefined)
   }
}


const n = +process.argv[2]
const minDepth = 4
const maxDepth = Math.max(minDepth + 2, n);
const stretchDepth = maxDepth + 1

let check = bottomUpTree(stretchDepth).check()
console.log("stretch tree of depth " + stretchDepth + "\t check: " + check)

const longLivedTree = bottomUpTree(maxDepth)
for (let depth=minDepth; depth<=maxDepth; depth+=2) {
   let iterations = 1 << (maxDepth - depth + minDepth)

   check = 0;
   for (let i=1; i<=iterations; i++) {
      check += bottomUpTree(depth).check()
   }
   console.log(iterations + "\t trees of depth " + depth + "\t check: " + check)
}
console.log("long lived tree of depth " + maxDepth + "\t check: " + longLivedTree.check())
