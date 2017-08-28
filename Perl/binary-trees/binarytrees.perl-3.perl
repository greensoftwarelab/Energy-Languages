# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org/
#
# contributed by Emanuele Zeppieri
# modified by Christian Walde (threads)
# *reset* by A. Sinan Unur

use threads;

run( @ARGV );

sub bottomup_tree {
    my $depth = shift;
    return 0 unless $depth;
    --$depth;
    [ bottomup_tree($depth), bottomup_tree($depth) ];
}

sub check_tree {
    return 1 unless ref $_[0];
    1 + check_tree($_[0][0]) + check_tree($_[0][1]);
}

sub stretch_tree {
    my $stretch_depth = shift;
    my $stretch_tree = bottomup_tree($stretch_depth);
    print "stretch tree of depth $stretch_depth\t check: ",
    check_tree($stretch_tree), "\n";
}

sub depth_iteration {
    my ( $depth, $max_depth, $min_depth ) = @_;

    my $iterations = 2**($max_depth - $depth + $min_depth);
    my $check      = 0;

    foreach ( 1 .. $iterations ) {
        $check += check_tree( bottomup_tree( $depth ) );
    }

    return ( $depth => [ $iterations, $depth, $check ] );
}

sub run {
    my $max_depth = shift;
    my $min_depth = 4;

    $max_depth = $min_depth + 2 if $min_depth + 2 > $max_depth;

    stretch_tree( $max_depth + 1 );

    my $longlived_tree = bottomup_tree( $max_depth );

    my @results;
    for ( my $depth = $min_depth ; $depth <= $max_depth ; $depth += 2 ) {
        while ( 1 ) {
            last if threads->list < 4;
            push @results, $_->join for threads->list( threads::joinable );
        }
        threads->create(
            { 'context' => 'list', 'stack_size' => 64 },
            sub { depth_iteration( $depth, $max_depth, $min_depth ) }
        );
    }
    while ( threads->list ) {
        push @results, $_->join for threads->list( threads::joinable );
    }

    my %results = @results;
    for my $key ( sort { $a <=> $b } keys %results ) {
        my ( $iterations, $depth, $check ) = @{ $results{$key} };
        print $iterations, "\t trees of depth $depth\t check: ", $check, "\n";
    }

    print "long lived tree of depth $max_depth\t check: ",
        check_tree( $longlived_tree ), "\n";
}