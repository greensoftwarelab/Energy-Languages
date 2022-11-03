-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -fopenmp -mfpmath=sse -msse3 nbody.cpp -o nbody_run
	
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody_run 50000000" C++ n-body

run:
	./nbody_run 50000000

mem:
	/usr/bin/time -v ./nbody_run 50000000

valgrind:
	valgrind --tool=massif --stacks=yes ./nbody_run 50000000
