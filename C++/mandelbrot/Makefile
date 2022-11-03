-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -mfpmath=sse -msse2 -fopenmp -mno-fma mandelbrot.cpp -o mandelbrot_run
	
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot_run 16000" C++ mandelbrot

run:
	./mandelbrot_run 16000

mem:
	/usr/bin/time -v ./mandelbrot_run 16000

valgrind:
	valgrind --tool=massif --stacks=yes ./mandelbrot_run 16000
