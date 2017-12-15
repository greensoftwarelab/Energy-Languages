compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/perl-5.24.0/bin/perl spectralnorm.perl-4.perl 5500" Perl spectral-norm

run:
	/usr/local/src/perl-5.24.0/bin/perl spectralnorm.perl-4.perl 5500

mem:
	/usr/bin/time -v  /usr/local/src/perl-5.24.0/bin/perl spectralnorm.perl-4.perl 5500

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/perl-5.24.0/bin/perl spectralnorm.perl-4.perl 5500
