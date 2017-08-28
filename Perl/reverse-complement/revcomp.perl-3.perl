# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org/
#
# Contributed by David Eccles (gringer)

use strict;
use feature 'say';

local $/ = ">";
while (my $entry = <STDIN>) {
   chomp $entry;

   my ($header, $seq) = split /\n/, $entry, 2;
   next unless $header;

   {
      local $/ = "\n";
      say ">", $header;

      $seq =  reverse $seq;
      $seq =~ tr{wsatugcyrkmbdhvnATUGCYRKMBDHV\n}
              {WSTAACGRYMKVHDBNTAACGRYMKVHDB}d;

		for(my $pos = 0; $pos < length($seq); $pos += 60){
			say substr($seq, $pos, 60);
		}
   }
}