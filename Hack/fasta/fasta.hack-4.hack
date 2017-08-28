<?hh
/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Wing-Chung Leung
   modified by Isaac Gouy
   modified by anon
   modified by Ludovic Urbain
   PHP as HHVM/Hack by Isaac Gouy
 */

ob_implicit_flush(1);
ob_start(NULL, 4096);

$last = 42.0;
function gen_random(&$last, &$randoms, $max = 1.0, $ia = 3877.0, $ic = 29573.0, $im = 139968.0) {//2187 *67
   foreach($randoms as &$r) {
      $r = ($last = ($last * $ia + $ic) % $im) * $max / $im ; // at this point this is responsible for 50% of the CPU time. *may* be an area to address, but how ?
   }
}


/* Generate and write FASTA format */

function makeRandomFasta2($n) {
   $width = 60;
   $lines = (int) ($n / $width);
   $pick = "";
   $randoms = array_fill(0, $width, 0.0);
   global $last;

   // full lines
   for ($i = 0; $i < $lines; ++$i) {
      $pick = "";
      gen_random($last, $randoms);
      $j = 0;
      foreach ($randoms as $r) {
         if($r<0.302954942668){
            $pick.='a';
         }elseif($r<0.5009432431601){
            $pick.='c';
         }elseif($r<0.6984905497992){
            $pick.='g';
         }else{
            $pick.='t';
         }
      }
      echo $pick."\n";
   }

   // last, partial line
   $w = $n % $width;
   if ($w !== 0) {
      $pick = "";
      $randoms = array_fill(0, $w, 0.0);
      gen_random($last, $randoms);
      $j = 0;
      foreach ($randoms as $r) {
         if($r<0.302954942668){
            $pick.='a';
         }elseif($r<0.5009432431601){
            $pick.='c';
         }elseif($r<0.6984905497992){
            $pick.='g';
         }else{
            $pick.='t';
         }
      }
      echo $pick."\n";
   }
}


function makeRandomFasta3(&$genelist, $n) {
   $width = 60;
   $lines = (int) ($n / $width);
   $pick = "";
   $randoms = array_fill(0, $width, 0.0);
   global $last;
   // full lines
   for ($i = 0; $i < $lines; ++$i) {
      $pick = "";
      gen_random($last, $randoms);
      $j = 0;
      foreach ($randoms as $r) {
         $pick.=$genelist[(int)($r*100)];
      }
      echo $pick."\n";
   }
   // last, partial line
   $w = $n % $width;
   if ($w !== 0) {
      $pick = "";
      $randoms = array_fill(0, $w, 0.0);
      gen_random($last, $randoms);
      $j = 0;
     foreach ($randoms as $r) {
        $pick.=$genelist[(int)($r*100)];
     }
     echo $pick."\n";
   }
}


function makeRepeatFasta($s, $n) {
   $i = 0; $sLength = strlen($s); $lineLength = 60;
   while ($n > 0) {
      if ($n < $lineLength) $lineLength = $n;
      if ($i + $lineLength < $sLength){
         echo substr($s,$i,$lineLength)."\n";
         $i += $lineLength;
      } else {
         echo substr($s,$i);
         $i = $lineLength - ($sLength - $i);
         echo substr($s,0,$i)."\n";
      }
      $n -= $lineLength;
   }
}


/* Main -- define alphabets, make 3 fragments */

$iub=array(
   'a' => 0.27,
   'c' => 0.12,
   'g' => 0.12,
   't' => 0.27,

   'B' => 0.02,
   'D' => 0.02,
   'H' => 0.02,
   'K' => 0.02,
   'M' => 0.02,
   'N' => 0.02,
   'R' => 0.02,
   'S' => 0.02,
   'V' => 0.02,
   'W' => 0.02,
   'Y' => 0.02
);

function buildDafuqPercentArray($arr){
   $i=0;
   $r=array();
   foreach($arr as $k => $v){
      for($j=0;$j<$v*100;$j++){
         $r[]=$k;
      }
   }
   return $r;
}

$homosapiens = array(
   'a' => 0.3029549426680,
   'c' => 0.1979883004921,
   'g' => 0.1975473066391,
   't' => 0.3015094502008
);

$alu =
   'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' .
   'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' .
   'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' .
   'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' .
   'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' .
   'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' .
   'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA';

$n = 1000;

if ($_SERVER['argc'] > 1) $n = $_SERVER['argv'][1];

$iub2=buildDafuqPercentArray($iub);

echo ">ONE Homo sapiens alu\n";
makeRepeatFasta($alu, $n*2);

echo ">TWO IUB ambiguity codes\n";
makeRandomFasta3($iub2, $n*3);

echo ">THREE Homo sapiens frequency\n";
makeRandomFasta2($n*5);
