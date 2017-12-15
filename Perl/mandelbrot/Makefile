compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/perl-5.24.0/bin/perl mandelbrot.perl 16000" Perl mandelbrot

run:
	/usr/local/src/perl-5.24.0/bin/perl mandelbrot.perl 16000

mem:
	/usr/bin/time -v /usr/local/src/perl-5.24.0/bin/perl mandelbrot.perl 16000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/perl-5.24.0/bin/perl mandelbrot.perl 16000
