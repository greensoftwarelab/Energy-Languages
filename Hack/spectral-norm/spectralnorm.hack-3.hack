<?hh 
/* 
   The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Isaac Gouy
   multicore by anon
   PHP as HHVM/Hack by Isaac Gouy
 */


function A(&$i, &$j){
   return 1.0 / ( ( ( ($i+$j) * ($i+$j+1) ) >> 1 ) + $i + 1 );
}

function Av(&$n,&$v,&$start,&$end){
   global $_tpl;
   $Av = $_tpl;
   for ($i = $start; $i < $end; ++$i) {
      $sum = 0.0;
      foreach($v as $j=>$v_j) {
         $sum += A($i,$j) * $v_j;
      }
      $Av[$i] = $sum;
   }
   return $Av;
}

function Atv(&$n,&$v,&$start,&$end){
   global $_tpl;
   $Atv = $_tpl;
   for($i = $start; $i < $end; ++$i) {
      $sum = 0.0;
      foreach($v as $j=>$v_j) {
         $sum += A($j,$i) * $v_j;
      }
      $Atv[$i] = $sum;
   }
   return $Atv;
}

function AtAv(&$n,&$v,&$start,&$end,&$sync){

   $tmp = Av($n, $v, $start, $end);
   if ($sync) sync($tmp);

   $tmp = Atv($n, $tmp, $start, $end);
   if ($sync) sync($tmp);

   return $tmp;
}

function sync(&$tmp) {

   global $parent,$chunk_data_size,$total_data_size,$pipe,$pipes;

   if (!$parent) {
      array_unshift($tmp, 'd*');
      $data = call_user_func_array('pack', $tmp);
      safe_write($pipe, $data);
      $tmp = array_merge(array(), unpack('d*', safe_read($pipe, $total_data_size)));
   } else {
      $tmps = array(array('d*'));
      foreach($pipes as $pipe) {
         $tmps[] = unpack('d*', safe_read($pipe, $chunk_data_size));
      }
      $tmps[] = &$tmp;
      $tmp = call_user_func_array('array_merge', $tmps);

      $data = call_user_func_array('pack', $tmp);
      foreach($pipes as $pipe) {
         safe_write($pipe, $data);
      }
      array_shift($tmp);
   }
}

function safe_write($fd, $data) {
   $len = strlen($data);
   do {
      $w = fwrite($fd, $data);
      $len -= $w;
   } while($len && ($data = substr($data, $w)) !== FALSE);
}
function safe_read($fd, $len) {
   $data = '';
   while ($len > 0) {
      $d = fread($fd, $len);
      $len -= strlen($d);
      $data .= $d;
   }
   return $data;
}
function pipe() {
   return stream_socket_pair(STREAM_PF_UNIX, STREAM_SOCK_STREAM, 0);
}


$n = (int) (($argc == 2) ? $argv[1] : 1);

$procs = 1;
if (file_exists('/proc/cpuinfo')) {
   $procs = preg_match_all('/^processor\s/m', file_get_contents('/proc/cpuinfo'), $discard);
}

if ($n < $procs) {
   $procs = 1;
}

$chunk_size = (int) ($n / $procs);
$double_size = strlen(pack('d', 0.0));
$chunk_data_size = $double_size * $chunk_size;
$total_data_size = $double_size * $n;

$pipes = array();
$parent = FALSE;
for($i = 0; $i < $procs; ++$i) {
   $range_begin = $i * $chunk_size;
   if ($i < ($procs - 1)) {
      $pipe = pipe();
      $pipes[] = $pipe[0];
      $pipe = $pipe[1];
      $range_end = $range_begin + $chunk_size;
      $pid = pcntl_fork();
      if ($pid === -1) {
         die('could not fork');
      } else if ($pid) {
         continue;
      }
      break;
   } else {
      $range_end = $n;
      $parent = TRUE;
   }
}

$u = array_fill(0, $n, 1.0);
$_tpl = array_fill($range_begin, $range_end - $range_begin, 0.0);
$sync = $procs > 0;

for ($i=0; $i<10; $i++){
   $v = AtAv($n,$u,$range_begin,$range_end,$sync);
   $u = AtAv($n,$v,$range_begin,$range_end,$sync);
}

if (!$parent) {
   exit(0);
}

$childs = $procs - 1;
while ($childs--) {
   pcntl_wait($s);
}

$vBv = 0.0;
$vv = 0.0;
$i = 0;
foreach($v as $val) {
   $vBv += $u[$i]*$val;
   $vv += $val*$val;
   ++$i;
}
printf("%0.9f\n", sqrt($vBv/$vv));