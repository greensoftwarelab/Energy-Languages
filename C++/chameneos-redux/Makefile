-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -pthread chameneosredux.cpp -o chameneosredux_run

run:
	./chameneosredux_run 6000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./chameneosredux_run 6000000
