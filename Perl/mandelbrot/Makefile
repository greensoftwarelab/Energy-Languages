compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PERL) mandelbrot.perl 16000" Perl mandelbrot

run:
	$(PERL) mandelbrot.perl 16000

mem:
	/usr/bin/time -v $(PERL) mandelbrot.perl 16000

valgrind:
	valgrind --tool=massif --stacks=yes $(PERL) mandelbrot.perl 16000
