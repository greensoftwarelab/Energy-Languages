#  The Computer Language Benchmarks Game
#  http://benchmarksgame.alioth.debian.org/

#  contributed by Karl FORNER
# (borrowed fasta loading routine from Kjetil Skotheim, 2005-11-29)
# Corrected again by Jesse Millikan
# revised by Kuang-che Wu
# Multi-threaded by Andrew Rodland

use strict;
use threads;

my $threads = num_cpus() || 1;

my ($sequence, $begin, $end);
$/ = ">";
/^THREE/ and $sequence = uc(join "", grep !/^THREE/, split /\n+/) while <STDIN>;

my ($l,%h,$sum) = (length $sequence);

foreach my $frame (1,2) {
  %h = ();
  update_hash_for_frame($frame);
  $sum = $l - $frame + 1;
  printf "$_ %.3f\n", $h{$_}*100/$sum for sort { $h{$b} <=> $h{$a} || $a cmp $b } keys %h;
  print "\n";
}

foreach my $s (qw(GGT GGTA GGTATT GGTATTTTAATT GGTATTTTAATTTATAGT)) {
  update_hash_for_frame(length($s));
  printf "%d\t$s\n", $h{$s};
}

sub update_hash_for_frame {
  my $frame = $_[0];
  my @threads;
  for my $i (0 .. $threads - 1) {
    use integer;
    my $begin = $l * $i / $threads;
    my $end = $l * ($i + 1) / $threads - 1;
    no integer;
    if ($end > $l - $frame) {
      $end = $l - $frame;
    }
    push @threads, threads->create(\&update_hash_slice, $frame, $begin, $end);
  }
  for my $thread (@threads) {
    my $count = $thread->join;
    $h{$_} += $count->{$_} for keys %$count;
  }
}

sub update_hash_slice {
  my ($frame, $begin, $end) = @_;
  my %local;
  $local{substr($sequence,$_,$frame)}++ for $begin .. $end;
  return \%local;
}

sub num_cpus {
  open my $fh, '</proc/cpuinfo' or return;
  my $cpus;
  while (<$fh>) {
    $cpus ++ if /^processor\s+:/;
  }
  return $cpus;
}