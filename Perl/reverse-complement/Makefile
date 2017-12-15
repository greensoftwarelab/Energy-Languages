compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/perl-5.24.0/bin/perl revcomp.perl-3.perl 0 < ../../revcomp-input25000000.txt" Perl reverse-complement

run:
	/usr/local/src/perl-5.24.0/bin/perl revcomp.perl-3.perl 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/perl-5.24.0/bin/perl revcomp.perl-3.perl 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/perl-5.24.0/bin/perl revcomp.perl-3.perl 0 < ../../revcomp-input25000000.txt
