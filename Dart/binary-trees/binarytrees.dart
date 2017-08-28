/* The Computer Language Benchmarks game
   http://benchmarksgame.alioth.debian.org/

   contributed by Jos Hirth, transliterated from Jarkko Miettinen's Java program
   *reset*
*/

final int minDepth = 4;

void main(args){
  int n = args.length > 0 ? int.parse(args[0]) : 0;

  int maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;
  int stretchDepth = maxDepth + 1;

  int check = (TreeNode.bottomUpTree(stretchDepth)).itemCheck();
  print("stretch tree of depth $stretchDepth\t check: $check");

  TreeNode longLivedTree = TreeNode.bottomUpTree(maxDepth);

  for (int depth = minDepth; depth <= maxDepth; depth += 2){
    int iterations = 1 << (maxDepth - depth + minDepth);
    check = 0;

    for (int i = 1; i <= iterations; i++){
      check += (TreeNode.bottomUpTree(depth)).itemCheck();
    }
    print("${iterations}\t trees of depth $depth\t check: $check");
  }
  print("long lived tree of depth $maxDepth\t check: ${longLivedTree.itemCheck()}");
}


class TreeNode{
  TreeNode left, right;

  TreeNode([this.left, this.right]);

  static TreeNode bottomUpTree(int depth){
    if (depth > 0){
      return new TreeNode(
        bottomUpTree(depth - 1),
        bottomUpTree(depth - 1)
      );
    }
    return new TreeNode();
  }

  int itemCheck(){
    if (left == null){
      return 1;
    }
    return 1 + left.itemCheck() + right.itemCheck();
  }
}