# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org/
#
# contributed by A. Sinan Unur
#

# This version forks three children which each do a third of
# the matching work. Parent does the substitution work, collects
# results, and prints them. It has the advantage of working on
# non-threaded perls.

use strict;

use constant N_MATCH_WORKERS => 3;

use IO::Select;

run();

sub run {
    my @variants = qw/
           agggtaaa|tttaccct
       [cgt]gggtaaa|tttaccc[acg]
       a[act]ggtaaa|tttacc[agt]t
       ag[act]gtaaa|tttac[agt]ct
       agg[act]taaa|ttta[agt]cct
       aggg[acg]aaa|ttt[cgt]ccct
       agggt[cgt]aa|tt[acg]accct
       agggta[cgt]a|t[acg]taccct
       agggtaa[cgt]|[acg]ttaccct
    /;

    my @variants_re = map qr/$_/xiaa, @variants;

    my @iub = (
        [ 'tHa [Nt]',                 '<4>' ],
        [ 'aND | caN | Ha[DS] | WaS', '<3>' ],
        [ 'a [NSt] | BY',             '<2>' ],
        [ '< [^>]* >',                '|'   ],
        [ '\| [^|] [^|]* \|',         '-'   ],
    );

    my $seq = do { local $/; <STDIN> };

    my @report = (length $seq);

    $seq =~ s/>.*\n|\n//g;

    push @report, length( $seq );

    my %readers;

    for my $worker (1 .. N_MATCH_WORKERS) {
        pipe(my $reader, my $writer);
        my $pid = fork;
        if ( $pid ) {
            $readers{ $pid } = $reader;
            close $writer
                or die "Failed to close worker $worker's writer in parent: $!";
        }
        else {
            die "Fork failed: $?" unless defined $pid;
            close $reader
                or die "Failed to close parent's reader in worker $worker: $!";
            for (N_MATCH_WORKERS*($worker - 1) .. (N_MATCH_WORKERS*$worker - 1)) {
                printf $writer "%s\t%d\n", $variants[$_],
                    scalar( () = $seq =~ /$variants_re[$_]/g );
            }
            close $writer
                or die "Failed to close worker ${worker}'s writer in worker: $!";
            exit( 0 );
        }
    }

    # do our own work
    $seq =~ s/$_->[0]/$_->[1]/gx for @iub;
    push @report, length( $seq );
    unshift @report, '';

    # collect output from match workers
    my %match_results;

    for my $reader ( values %readers ) {
        while (<$reader>) {
            chomp;
            my ($v, $n) = split /\t/;
            $match_results{ $v } = $n;
        }
        close $reader
            or die "Failed to close reader in parent: $!";
    }

    waitpid($_, 0) for keys %readers;

    unshift @report, map "$variants[$_] $match_results{ $variants[$_] }",
        0 .. $#variants;
    print join("\n", @report), "\n";
}