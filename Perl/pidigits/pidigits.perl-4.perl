# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org/
#
#   contributed by Robert Bradshaw
#   modified by Ruud H.G.van Tol
#   modified by Emanuele Zeppieri
#   modified to use Math:GMP by Kuang-che Wu
#   modified by Kjetil Skotheim, cleanup + speedup 10%

use Math::GMP;
my $n = $ARGV[0];
my($z0, $z1, $z2) = map Math::GMP->new($_), 1,0,1;
my($k, $s, $d, $f);
for (1..$n) {
    while ( $z0>$z2 or ( $d = ($f=$z0*3+$z1)/$z2 ) != ($f+$z0)/$z2 ) {
        $z2 *= $f = 2*++$k+1;
        $z1  = $f * ($z1+2*$z0);
        $z0 *= $k;
    }
    $z1  = 10 * ($z1-$d*$z2);
    $z0 *= 10;
    $s  .= $d;
    printf "%-10s\t:$_\n", $s and $s="" if $_%10<1 or $_==$n;
}