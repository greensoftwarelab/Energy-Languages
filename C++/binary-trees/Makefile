-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) $(shell pkg-config --cflags apr-1) -fopenmp binarytrees.cpp -o binarytrees_run $(shell pkg-config --libs apr-1)

run:
	./binarytrees_run 21

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) './binarytrees_run 21'
