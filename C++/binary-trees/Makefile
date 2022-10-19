compile:
	$(CXX) -pipe -O3 -fomit-frame-pointer -march=native -std=c++14 -fopenmp -I/usr/include/apr-1.0 binarytrees.gpp-9.c++ -o binarytrees.gpp-9.gpp_run -lapr-1

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.gpp-9.gpp_run 21" C++ binary-trees

run:
	./binarytrees.gpp-9.gpp_run 21

mem:
	/usr/bin/time -v ./binarytrees.gpp-9.gpp_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees.gpp-9.gpp_run 21
