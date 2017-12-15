compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/perl-5.24.0/bin/perl fasta.perl 25000000" Perl fasta

run:
	/usr/local/src/perl-5.24.0/bin/perl fasta.perl 25000000
mem:
	/usr/bin/time -v /usr/local/src/perl-5.24.0/bin/perl fasta.perl 25000000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/perl-5.24.0/bin/perl fasta.perl 25000000

