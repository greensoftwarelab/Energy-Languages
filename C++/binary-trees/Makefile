-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) $(shell pkg-config --cflags apr-1) -fopenmp binarytrees.cpp -o binarytrees_run $(shell pkg-config --libs apr-1)

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees_run 21" C++ binary-trees

run:
	./binarytrees_run 21

mem:
	/usr/bin/time -v ./binarytrees_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees_run 21
