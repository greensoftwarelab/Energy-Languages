# The Computer Language Benchmarks Game
#  http://benchmarksgame.alioth.debian.org/
#
#  contributed by Mykola Zubach

use strict;
use threads;
use threads::shared;

use constant MAXITER => 50;
use constant LIMIT => 4.0;
use constant XMIN => -1.5;
use constant YMIN => -1;
use constant WHITE => "\0";
use constant BLACK => "\001";

my ($w, $h, @threads, $invN);
my @jobs :shared;
my @picture :shared;

sub draw_line($) {
   my $y = shift;
   my $line;
   my $Ci = $y * $invN + YMIN;
X:
   for my $x (0 .. $w - 1) {
      my ($Zr, $Zi, $Tr, $Ti);
      my $Cr = $x * $invN + XMIN;

      for (1 .. MAXITER) {
         $Zi = $Zi * 2 * $Zr + $Ci;
         $Zr = $Tr - $Ti + $Cr;
         $Ti = $Zi * $Zi;
         $Tr = $Zr * $Zr;
         if ($Tr + $Ti > LIMIT) {
            $line .= WHITE;
            next X;
         }
      }
      $line .= BLACK;
   }
   $picture[$y] = pack 'B*', $line;
}

sub process_queue() {
   while(defined(my $y = pop @jobs)) {
      draw_line($y);
   }
}

sub num_cpus() {
   open my $fh, '</proc/cpuinfo' or return 4;
   my $cpus;
   while(<$fh>) {
      $cpus ++ if /^processor\s+:/;
   }
   return $cpus;
}

## MAIN()

$w = $h = shift || 200;
$invN = 2 / $w;
@jobs = (0 .. $h - 1);

for (1 .. num_cpus()) {
   push @threads, threads->create(\&process_queue);
}

for (@threads) {
   $_->join;
}

print "P4\n$w $h\n"; # PBM image header
print @picture;