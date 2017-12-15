compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "perl pidigits.perl-4.perl 10000" Perl pidigits

run:
	perl pidigits.perl-4.perl 10000

mem:
	/usr/bin/time -v perl pidigits.perl-4.perl 10000

valgrind:
	valgrind --tool=massif --stacks=yes perl pidigits.perl-4.perl 10000
