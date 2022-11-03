-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -mfpmath=sse -msse2 -fopenmp spectralnorm.cpp -o spectralnorm_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm_run 5500" C++ spectral-norm

run:
	./spectralnorm_run 5500

mem:
	/usr/bin/time -v ./spectralnorm_run 5500

valgrind:
	valgrind --tool=massif --stacks=yes ./spectralnorm_run 5500
