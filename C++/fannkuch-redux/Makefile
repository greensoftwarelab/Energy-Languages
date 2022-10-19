compile:
	$(CXX) -pipe -O3 -fomit-frame-pointer -march=native -std=c++11 -fopenmp fannkuchredux.gpp-5.c++ -o fannkuchredux.gpp-5.gpp_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.gpp-5.gpp_run 12" C++ fannkuch-redux

run:
	./fannkuchredux.gpp-5.gpp_run 12

mem:
	/usr/bin/time -v  ./fannkuchredux.gpp-5.gpp_run 12

valgrind:
	valgrind --tool=massif --stacks=yes  ./fannkuchredux.gpp-5.gpp_run 12
