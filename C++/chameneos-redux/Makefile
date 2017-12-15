compile:
	/usr/bin/g++ -c -pipe -O3 -fomit-frame-pointer -march=native  --std=c++11 -pthread chameneosredux.gpp-5.c++ -o chameneosredux.gpp-5.c++.o && /usr/bin/g++ chameneosredux.gpp-5.c++.o -o chameneosredux.gpp-5.gpp_run -Wl,--no-as-needed -lpthread 

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./chameneosredux.gpp-5.gpp_run 6000000"

run:
	./chameneosredux.gpp-5.gpp_run 6000000

mem:
	/usr/bin/time -v ./chameneosredux.gpp-5.gpp_run 6000000

valgrind:
	valgrind --tool=massif --stacks=yes ./chameneosredux.gpp-5.gpp_run 6000000
