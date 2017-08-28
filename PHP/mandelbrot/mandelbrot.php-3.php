<?php
/* 
   The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Thomas GODART (based on Greg Buchholz's C program)
   multicore by anon
 */

function getProcs() {
   $procs = 1;
   if (file_exists('/proc/cpuinfo')) {
      $procs = preg_match_all('/^processor\s/m', file_get_contents('/proc/cpuinfo'), $discard);
   }
   $procs <<= 1;
   return $procs;
}

$h = (int) (($argc == 2) ? $argv[1] : 600);
$w = $h;

if ($w % 8) {
   fprintf(STDERR, "width %d not multiple of 8\n", $w);
   exit(1);
}

printf ("P4\n%d %d\n", $w, $h);

$shsize = $w * ($w >> 3);
$shmop = shmop_open(ftok(__FILE__, chr(time() & 255)), 'c', 0644, $shsize);

if (!$shmop) {
   echo "faild to shmop_open()\n";
   exit(1);
}

$bit_num = 128;
$byte_acc = 0;

$yfac = 2.0 / $h;
$xfac = 2.0 / $w;

$shifted_w = $w >> 3;
$step = 1;

$procs = getProcs();
$child = $procs - 1;
while ($child > 0) {
   $pid = pcntl_fork();
   if ($pid === -1) {
      die('could not fork');
   } else if ($pid) {
      --$child;
      continue;
   }
   break;
}

$step = $procs;
$y = $child;

for ( ; $y < $h ; $y+=$step)
{
   $result = array('c*');

   $Ci = $y * $yfac - 1.0;

   for ($x = 0 ; $x < $w ; ++$x)
   {
      $Zr = 0; $Zi = 0; $Tr = 0; $Ti = 0.0;

      $Cr = $x * $xfac - 1.5;

      do {
         for ($i = 0 ; $i < 50 ; ++$i)
         {
            $Zi = 2.0 * $Zr * $Zi + $Ci;
            $Zr = $Tr - $Ti + $Cr;
            $Tr = $Zr * $Zr;
            if (($Tr+($Ti = $Zi * $Zi)) > 4.0) break 2;
         }
         $byte_acc += $bit_num;
      } while (FALSE);

      if ($bit_num === 1) {
         $result[] = $byte_acc;
         $bit_num = 128;
         $byte_acc = 0;
      } else {
         $bit_num >>= 1;
      }
   }
   if ($bit_num !== 128) {
      $result[] = $byte_acc;
      $bit_num = 128;
      $byte_acc = 0;
   }
   shmop_write($shmop, call_user_func_array('pack', $result), $y * $shifted_w);
}

if ($child > 0) {
   exit(0);
}

$child = $procs - 1;
$status = 0;
while ($child-- > 0) {
   pcntl_wait($status);
}

$step = $shsize >> 3;
for($i = 0; $i < $shsize; $i+=$step) {
   echo shmop_read($shmop, $i, $step);
}
shmop_delete($shmop);
?>

