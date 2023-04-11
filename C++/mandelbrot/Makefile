-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -mfpmath=sse -msse2 -fopenmp -mno-fma mandelbrot.cpp -o mandelbrot_run

run:
	./mandelbrot_run 16000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) './mandelbrot_run 16000'
