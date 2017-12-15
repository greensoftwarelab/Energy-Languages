compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/perl-5.24.0/bin/perl nbody.perl-2.perl 50000000" Perl n-body

run:
	/usr/local/src/perl-5.24.0/bin/perl nbody.perl-2.perl 50000000

mem:
	/usr/bin/time -v /usr/local/src/perl-5.24.0/bin/perl nbody.perl-2.perl 50000000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/perl-5.24.0/bin/perl nbody.perl-2.perl 50000000
