-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -fopenmp fannkuchredux.cpp -o fannkuchredux_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux_run 12" C++ fannkuch-redux

run:
	./fannkuchredux_run 12

mem:
	/usr/bin/time -v  ./fannkuchredux_run 12

valgrind:
	valgrind --tool=massif --stacks=yes  ./fannkuchredux_run 12
