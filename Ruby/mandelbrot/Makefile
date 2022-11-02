compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RUBY) -W0 mandelbrot.yarv-5.yarv 16000" Ruby mandelbrot

run:
	$(RUBY) -W0 mandelbrot.yarv-5.yarv 16000

mem:
	/usr/bin/time -v $(RUBY) -W0 mandelbrot.yarv-5.yarv 16000

valgrind:
	valgrind --tool=massif --stacks=yes $(RUBY) -W0 mandelbrot.yarv-5.yarv 16000
