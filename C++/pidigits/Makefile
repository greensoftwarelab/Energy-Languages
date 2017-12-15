compile:
	/usr/bin/g++ -c -pipe -O3 -fomit-frame-pointer -march=native  -std=c++14 -g pidigits.gpp-4.c++ -o pidigits.gpp-4.c++.o &&  /usr/bin/g++ pidigits.gpp-4.c++.o -o pidigits.gpp-4.gpp_run -lgmp -lgmpxx 
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits.gpp-4.gpp_run 10000" C++ pidigits

run:
	./pidigits.gpp-4.gpp_run 10000

mem:
	/usr/bin/time -v ./pidigits.gpp-4.gpp_run 10000

valgrind:
	valgrind --tool=massif --stacks=yes ./pidigits.gpp-4.gpp_run 10000
