-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) pidigits.cpp -o pidigits_run -lgmp -lgmpxx
	
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits_run 10000" C++ pidigits

run:
	./pidigits_run 10000

mem:
	/usr/bin/time -v ./pidigits_run 10000

valgrind:
	valgrind --tool=massif --stacks=yes ./pidigits_run 10000
