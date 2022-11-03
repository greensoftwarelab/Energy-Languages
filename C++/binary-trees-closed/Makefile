-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) binarytrees.cpp -o binarytrees_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees_run 21" C++ binary-trees

run:
	./binarytrees_run 21

mem:
	/usr/bin/time -v ./binarytrees_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees_run 21
