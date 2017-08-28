<?hh
/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Peter Baltruschat
   modified by Levi Cameron
   multiprocessing by Yuriy Moskalchuk
*/

function createTree($depth)
{
    if (!$depth) {
        return [null, null];
    }
    $depth--;

    return [
        createTree($depth),
        createTree($depth)
    ];
}

function checkTree($treeNode)
{
    return 1
        + ($treeNode[0][0] === null ? 1 : checkTree($treeNode[0]))
        + ($treeNode[1][0] === null ? 1 : checkTree($treeNode[1]));
}

$minDepth = 4;
$n = ($argc == 2) ? $argv[1] : 1;
$maxDepth = max($minDepth + 2, $n);
$stretchDepth = $maxDepth + 1;

$stretchTree = createTree($stretchDepth);
echo "stretch tree of depth $stretchDepth\t check: ", checkTree($stretchTree), PHP_EOL;
unset($stretchTree);

$longLivedTree = createTree($maxDepth);

function startWorker($depth, $iter, $n, $size, $shmId)
{
    $pid = pcntl_fork();
    if ($pid) {
        return $pid;
    } else {
        $check = 0;
        for ($i = 1; $i <= $iter; ++$i) {
            $t = createTree($depth);
            $check += checkTree($t);
            unset($t);
        }
        $result = "$iter\t trees of depth $depth\t check: $check\n";
        shmop_write($shmId, $result, $size * ($n - 1));
        exit(0);
    }
}

$iterations = 1 << ($maxDepth);
$depthIterations = [$minDepth => $iterations];
do {
    $minDepth += 2;
    if ($minDepth <= $maxDepth) {
        $iterations >>= 2;
        $depthIterations[$minDepth] = $iterations;
    }
} while ($minDepth <= $maxDepth);

$size = 128;
$shmId = shmop_open(ftok(__FILE__, 'b'), 'c', 0644, $size * (count($depthIterations) + 1));
$PIDs = [];
$n = 0;
foreach ($depthIterations as $d => $i) {
    $PIDs[] = startWorker($d, $i, ++$n, $size, $shmId);
}
foreach ($PIDs as $PID) {
    pcntl_waitpid($PID, $status);
}
foreach (range(1, count($depthIterations)) as $n) {
    echo trim(shmop_read($shmId, $size * ($n - 1), $size)), PHP_EOL;
}
shmop_delete($shmId);

echo "long lived tree of depth $maxDepth\t check: ", checkTree($longLivedTree), PHP_EOL;