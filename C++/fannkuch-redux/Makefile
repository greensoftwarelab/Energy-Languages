-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -fopenmp fannkuchredux.cpp -o fannkuchredux_run

run:
	./fannkuchredux_run 12

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./fannkuchredux_run 12
