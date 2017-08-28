<?hh 
/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   
   multicore version 
   algorithm is based on Java 6 source code by Oleg Mazurov
   fork/shared mem is based on mandelbrot.php-3 
   contributed by Oleksii Prudkyi
   PHP as HHVM/Hack by Isaac Gouy
*/

$n = (int) $argv[1];
$count_base = array();
$p_base = range(0, $n - 1);

$procs = 1;
if (file_exists('/proc/cpuinfo')) {
  $procs = preg_match_all('/^processor\s/m', file_get_contents('/proc/cpuinfo'), $discard);
}
$procs <<= 1;

$Fact = array(1);
for ($i = 1, $j = 0; $i <= $n; $j = $i++) {
   $Fact[$i] = $Fact[$j] * $i;
}

$index = 0;
$index_max = $Fact[$n];
$index_step = intval(($index_max + $procs-1) / $procs);

$shsize = $procs * 16;
$shmop = shmop_open(ftok(__FILE__, chr(time() & 255)), 'c', 0644, $shsize);

if (!$shmop) {
   echo "faild to shmop_open()\n";
   exit(1);
}

for ($proc = 0; $proc < $procs; ++$proc, $index += $index_step ) {
   
   if($proc < $procs - 1) {
      $pid = pcntl_fork();
      if ($pid === -1) {
         die('could not fork');
      } else if ($pid) {
         continue;
      }
   }

   $idxMin = $index;
   $idxMax = min( $index_max, $index + $index_step );

   //firstPermutation( idxMin );
   $count = $count_base;
   $p = $p_base;
   $idx = $idxMin;
   for ($i = $n - 1; $i > 0; --$i ) {
      $factI = $Fact[$i];
      $reminder = $idx % $factI;
      $d = (($idx- $reminder) / $factI);
      $count[$i] = $d;
      $idx = $reminder;

      $pp = $p;
      for ($j=0, $jd = $d; $j<=$i; ++$j, ++$jd ) {
         $p[$j] = $pp[($jd <= $i) ? $jd :$jd-$i-1];
      }
   }

   $maxflips = 1;
   $chksum = 0;
   $sign = 1;
   for ($idx =$idxMin;; ) {

      $first = $p[0];
      if ( $first != 0 ) {
         //int flips = countFlips();
         $flips = 1;
         if ( $p[$first] != 0 ) {
            $pp = $p;
            $p0 = $first;
            do {
               ++$flips;
               if ($p0 >= 3){
                  $i = 1; $j = $p0 - 1;
                  do { 
                     $t = $pp[$i]; 
                     $pp[$i] = $pp[$j]; 
                     $pp[$j] = $t; 
                     ++$i;
                     --$j;
                  } while ($i < $j); 
               }

               $t = $pp[$p0];
               $pp[$p0] = $p0;
               $p0 = $t;
            } while ( $pp[$p0] != 0 );
         }
         $maxflips = max( $maxflips, $flips );
         $chksum += $sign*$flips;
      }

      ++$idx ;
      if ( $idx  == ($idxMax) ) {
         break;
      }

      if ($sign == 1){
         $p[0] = $p[1];
         $p[1] = $first;
         $sign = -1;
      } else {
         $t = $p[1]; 
         $p[1] = $p[2]; 
         $p[2] = $t; 
         $sign = 1; 
         
         for($i=2;;){ 
            $sx = &$count[$i];
            ++$sx;
            if ($sx <= $i) {
               break; 
            } else {
               $sx = 0;
               for($j=0; $j<=$i; ){ $p[$j++] = $p[$j]; } 
               ++$i;
               $p[$i] = $first;
               $first = $p[0]; 
            }
         }
      }
   }
   $written_size = shmop_write($shmop, pack("ii", $maxflips, $chksum), $proc * 16);

   if($pid === 0) {
      exit(0);
   }
}

$child = $procs - 1;
$status = 0;
while ($child-- > 0) {
   pcntl_wait($status);
}

$offset = 0;
$res = 0;
$chk = 0;
for ($proc = 0; $proc < $procs; ++$proc, $offset += 16 ) {
   list($v, $chk_v) = array_values(unpack('ia/ib', shmop_read($shmop, $offset, $written_size)));
   $res = max( $res, $v );
   $chk += $chk_v;
}

printf("%d\nPfannkuchen(%d) = %d\n", $chk, $n, $res);