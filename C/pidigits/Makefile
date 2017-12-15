compile:
	/usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native  pidigits.c -o pidigits.gcc_run -lgmp
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits.gcc_run 10000" C pidigits

run:
	./pidigits.gcc_run 10000
mem:
	/usr/bin/time -v ./pidigits.gcc_run 10000
