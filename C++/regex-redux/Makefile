-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -fopenmp regexredux.cpp -o regexredux_run -lboost_regex

run:
	./regexredux_run 0 < ../../regexredux-input5000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./regexredux_run 0 < ../../regexredux-input5000000.txt
