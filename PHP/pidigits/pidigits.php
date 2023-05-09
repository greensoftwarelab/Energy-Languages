<?php 
/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Oleksii Prudkyi
   port from pidigits.lua-5.lua (Mike Pall, Wim Couwenberg)
   modified by Craig Russell
*/

$N = (int) $argv[1];

ob_implicit_flush(1);
ob_start(NULL, 4096);

$w = gmp_init(0);

$k = 1;
$n1 = gmp_init(4);
$n2 = gmp_init(3);
$d = gmp_init(1);

$i = 0;
while(true) {
   //digit
   $u = gmp_div_q($n1, $d);
   $v = gmp_div_q($n2, $d);
   if(gmp_cmp($u, $v) == 0) {
      echo gmp_strval($u);
      ++$i;
      if($i % 10 == 0) {
         echo "\t:" , $i , "\n";
      }
      if($i == $N) {
         break;
      }
      //extract
      $u = gmp_mul($d, gmp_mul(-10, $u));
      $n1 = gmp_mul($n1, 10);
      $n1 = gmp_add($n1, $u);
      $n2 = gmp_mul($n2, 10);
      $n2 = gmp_add($n2, $u);
   } else {
      //produce
      $k2 = $k << 1;
      $u = gmp_mul($n1, $k2 - 1);
      $v = gmp_add($n2, $n2);
      $w = gmp_mul($n1, $k - 1);
      $n1 = gmp_add($u, $v);
      $u = gmp_mul($n2, $k + 2);
      $n2 = gmp_add($w, $u);
      $d = gmp_mul($d, $k2 + 1);
      ++$k;
   }
}
if($i % 10 != 0) {
   echo str_repeat(' ', 10 - $N % 10), "\t:", $N, "\n";
}