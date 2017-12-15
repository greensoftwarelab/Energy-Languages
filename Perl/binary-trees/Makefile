compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/perl-5.24.0/bin/perl binarytrees.perl-3.perl 21" Perl binary-trees

run:
	/usr/local/src/perl-5.24.0/bin/perl binarytrees.perl-3.perl 21

mem:
	/usr/bin/time -v  /usr/local/src/perl-5.24.0/bin/perl binarytrees.perl-3.perl 21

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/perl-5.24.0/bin/perl binarytrees.perl-3.perl 21
