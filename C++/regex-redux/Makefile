-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -fopenmp regexredux.cpp -o regexredux_run -lboost_regex

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./regexredux_run 0 < ../../regexredux-input5000000.txt" C++ regex-redux

run:
	./regexredux_run 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v ./regexredux_run 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./regexredux_run 0 < ../../regexredux-input5000000.txt
