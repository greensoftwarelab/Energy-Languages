-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -fopenmp regexredux.cpp -o regexredux_run -lre2

run:
	./regexredux_run 0 < ../../regexredux-input5000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) ./regexredux_run 0 < ../../regexredux-input5000000.txt
