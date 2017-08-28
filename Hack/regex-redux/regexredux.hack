<?hh
/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   regex-dna program contributed by Danny Sauer
   modified by Josh Goldfoot
   modified by Sergey Khripunov
   modified by Craig Russell
   PHP as HHVM/Hack by Isaac Gouy
   converted from regex-dna program
*/

$tok = ftok(__FILE__, chr(time() & 255));
$queue = msg_get_queue($tok);

$variants = array(
    'agggtaaa|tttaccct',
    '[cgt]gggtaaa|tttaccc[acg]',
    'a[act]ggtaaa|tttacc[agt]t',
    'ag[act]gtaaa|tttac[agt]ct',
    'agg[act]taaa|ttta[agt]cct',
    'aggg[acg]aaa|ttt[cgt]ccct',
    'agggt[cgt]aa|tt[acg]accct',
    'agggta[cgt]a|t[acg]taccct',
    'agggtaa[cgt]|[acg]ttaccct',
);

// IUB replacement parallel arrays
$IUB = array();                 $IUBnew = array();
$IUB[]='/tHa[Nt]/S';            $IUBnew[]='<4>';
$IUB[]='/aND|caN|Ha[DS]|WaS/S'; $IUBnew[]='<3>';
$IUB[]='/a[NSt]|BY/S';          $IUBnew[]='<2>';
$IUB[]='/<[^>]*>/S';            $IUBnew[]='|';
$IUB[]='/\\|[^|][^|]*\\|/S';    $IUBnew[]='-';


// read in file
$contents = file_get_contents('php://stdin');
$initialLength = strlen($contents);

// remove things
$contents = preg_replace('/^>.*$|\n/mS', '', $contents);
$codeLength = strlen($contents);

// do regexp counts
$messages = array_flip($variants);
$workers = $results = array();
foreach ($variants as $key => $regex){
   if($key == 0 || $key == 2 || $key == 4 || $key == 6) {
      $pid = pcntl_fork();
      if($pid) $workers[] = $pid;
   }
   if($pid && $key > 7) {
      $messages[$regex] =
         preg_match_all('/' . $regex . '/iS', $contents, $discard);
   }
   else if(!$pid) {
      $results[] = $regex . ',' . 
         preg_match_all('/' . $regex . '/iS', $contents, $discard);
      if($key == 1 || $key == 3 || $key == 5 || $key == 7) {
         msg_send($queue, 2, implode(';', $results), false, false, $errno);
         exit;
	  }
   }
}

// receive and output the counts
pcntl_wait($status);
foreach($workers as $worker) {
   msg_receive($queue, 2, $msgtype, 4096, $message, false);
   $message = explode(';', $message, 3);
   foreach($message as $line) {
      $tmp = explode(',', $line, 2);
      $messages[$tmp[0]] = $tmp[1];
   }
}
foreach($messages as $regex => $count) {
   echo $regex, ' ', $count, "\n";
}

// do replacements
$contents = preg_replace($IUB, $IUBnew, $contents);

echo "\n",
      $initialLength, "\n",
      $codeLength, "\n",
      strlen($contents), "\n";