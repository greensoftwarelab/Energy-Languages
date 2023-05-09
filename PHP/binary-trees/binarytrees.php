<?php
/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Yuriy Moskalchuk
   (based on contributions of Peter Baltruschat and Levi Cameron)
*/

function createTree($depth)
{
    global $obj;
    if ($depth === 0) {
        return clone $obj;
    }
    $depth--;

    $t = clone $obj;
    $t->l = createTree($depth);
    $t->r = createTree($depth);

    return $t;
}

function checkTree(&$treeNode)
{
    $sum = 1
        + ($treeNode->l->l === null ? 1 : checkTree($treeNode->l))
        + ($treeNode->r->l === null ? 1 : checkTree($treeNode->r));
    unset($treeNode->l);
    unset($treeNode->r);

    return $sum;
}
$obj = new class {
    public $l;
    public $r;
};
$minDepth = 4;
$n = ($argc == 2) ? $argv[1] : 1;
$maxDepth = max($minDepth + 2, $n);
$stretchDepth = $maxDepth + 1;

$stretchTree = createTree($stretchDepth);
echo "stretch tree of depth $stretchDepth\t check: ", checkTree($stretchTree), PHP_EOL;
unset($stretchTree);

$longLivedTree = createTree($maxDepth);

$workersAvailable = file_exists('/proc/cpuinfo') ?
    preg_match_all('/^processor\s/m', file_get_contents('/proc/cpuinfo'), $null) - 1 : 1;
$workersAvailable = $workersAvailable ?: 1;

function startWorker($minDepth, $iterations, &$PIDs, $n, $size, $shmId)
{
    $pid = pcntl_fork();
    if ($pid) {
        $PIDs[$minDepth] = $pid;
    } else {
        $check = 0;
        for ($i = 1; $i <= $iterations; ++$i) {
            $t = createTree($minDepth);
            $check += checkTree($t);
            unset($t);
        }
        $result = "$iterations\t trees of depth $minDepth\t check: $check\n";
        shmop_write($shmId, $result, $size * ($n - 1));
        exit(0);
    }
}

function waitWorkers(&$PIDs, &$workersAvailable)
{
    foreach ($PIDs as $PID) {
        $pid = pcntl_waitpid($PID, $status);
        if ($pid) {
            unset($PIDs[array_search($pid, $PIDs)]);
            $workersAvailable++;
        }
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
extension_loaded('shmop') or dl('shmop.so');
$shmId = shmop_open(ftok(__FILE__, 'b'), 'c', 0644, $size * (count($depthIterations) + 1));
$workersPIDs = [];
$n = 0;
foreach ($depthIterations as $depth => $iteration) {
    if ($workersAvailable > 0) {
        work:
        startWorker($depth, $iteration, $workersPIDs, ++$n, $size, $shmId);
        $workersAvailable--;
    } else {
        waitWorkers($workersPIDs, $workersAvailable);
        if ($workersAvailable) {
            goto work;
        }
    }
}
waitWorkers($workersPIDs, $workersAvailable);

foreach (range(1, count($depthIterations)) as $n) {
    echo trim(shmop_read($shmId, $size * ($n - 1), $size)), PHP_EOL;
}
shmop_delete($shmId);

echo "long lived tree of depth $maxDepth\t check: ", checkTree($longLivedTree), PHP_EOL;