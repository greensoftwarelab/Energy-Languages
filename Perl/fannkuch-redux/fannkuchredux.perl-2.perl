# The Computer Language Benchmarks Game
#  http://benchmarksgame.alioth.debian.org/
#
#  contributed by Mykola Zubach

use strict;
use threads;
use integer;

my($max_flips, $chksum, $sign, $n, $p, $count, @threads, $thr, $t_chksum, $t_max_flips);

sub count_flips($) {
    my $p = shift;
    my $flips = 0;
    my($first);

# debug permutation order:
#print unpack('c*', $p), "\n";
    while($first = vec($p, 0, 8)) {
        $first ++;
        $flips ++;
        substr($p, 0, $first, reverse(substr($p, 0, $first)));
    }
    $max_flips = $flips if $flips > $max_flips;
    $chksum += $sign * $flips;
    $sign = -$sign;
}

sub fannkuchredux($$) {
    my $p = shift;
    my $rotate_len = shift;

# optimize case when rotation is equivalent to byte-swapping
    if($rotate_len == 3) {
        count_flips($p);
        substr($p, 0, 2, reverse(substr($p, 0, 2)));
        count_flips($p);
        substr($p, 1, 2, reverse(substr($p, 1, 2)));
        count_flips($p);
        substr($p, 0, 2, reverse(substr($p, 0, 2)));
        count_flips($p);
        substr($p, 1, 2, reverse(substr($p, 1, 2)));
        count_flips($p);
        substr($p, 0, 2, reverse(substr($p, 0, 2)));
        count_flips($p);
        return($chksum, $max_flips);
    } elsif($rotate_len == 2) {
        count_flips($p);
        substr($p, 0, 2, reverse(substr($p, 0, 2)));
        count_flips($p);
        return($chksum, $max_flips);
    } elsif($rotate_len == 1) {
        count_flips($p);
        return($chksum, $max_flips);
    }

    $rotate_len --;
    my $count = $rotate_len;
    for(;;) {
        fannkuchredux($p, $rotate_len);
        $count --;
        last if $count < 0;	# skip unneccessary rotation
        substr($p, 0, $rotate_len + 1) = substr($p, 1, $rotate_len) . substr($p, 0, 1);
    }
    return($chksum, $max_flips);
}

## MAIN()

$n = shift;
$p = pack('c*', (0 .. $n - 1));
$sign = 1;

$count = $n;
while($count > 0) {
    push @threads, threads->create(\&fannkuchredux, $p, $n-1);
    $p = substr($p, 1, $n-1) . substr($p, 0, 1);
    $count --;
}

foreach $thr (@threads) {
    ($t_chksum, $t_max_flips) = $thr->join();
    $chksum += $t_chksum;
    $max_flips = $t_max_flips if $max_flips < $t_max_flips;
}

print "$chksum\n";
print "Pfannkuchen($n) = $max_flips\n";