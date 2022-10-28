-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -mno-fma -fno-finite-math-only -mfpmath=sse -msse2 -fopenmp mandelbrot.c -o mandelbrot_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot_run 16000" C mandelbrot

run:
	./mandelbrot_run 16000

mem:
	/usr/bin/time -v ./mandelbrot_run 16000
