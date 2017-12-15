compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/perl-5.24.0/bin/perl knucleotide.perl 0 < knucleotide-input25000000.txt" Perl k-nucleotide

run:
	/usr/local/src/perl-5.24.0/bin/perl knucleotide.perl 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/perl-5.24.0/bin/perl knucleotide.perl 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/perl-5.24.0/bin/perl knucleotide.perl 0 < knucleotide-input25000000.txt
