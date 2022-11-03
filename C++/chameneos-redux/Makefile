-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -pthread chameneosredux.cpp -o chameneosredux_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./chameneosredux_run 6000000"

run:
	./chameneosredux_run 6000000

mem:
	/usr/bin/time -v ./chameneosredux_run 6000000

valgrind:
	valgrind --tool=massif --stacks=yes ./chameneosredux_run 6000000
