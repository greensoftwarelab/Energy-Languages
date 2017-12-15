compile:
	/usr/bin/g++ -c -pipe -O3 -fomit-frame-pointer -march=native  -std=c++11 -fopenmp fannkuchredux.gpp-5.c++ -o fannkuchredux.gpp-5.c++.o &&  /usr/bin/g++ fannkuchredux.gpp-5.c++.o -o fannkuchredux.gpp-5.gpp_run -fopenmp 
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.gpp-5.gpp_run 12" C++ fannkuch-redux

run:
	./fannkuchredux.gpp-5.gpp_run 12

mem:
	 /usr/bin/time -v  ./fannkuchredux.gpp-5.gpp_run 12
valgrind:
	valgrind --tool=massif --stacks=yes  ./fannkuchredux.gpp-5.gpp_run 12

