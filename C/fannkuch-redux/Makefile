compile:
	/usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -std=c99 -fopenmp fannkuchredux.gcc-5.c -o fannkuchredux.gcc-5.gcc_run 
	
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.gcc-5.gcc_run 12" C fannkuch-redux

run:
	./fannkuchredux.gcc-5.gcc_run 12

mem:
	 /usr/bin/time -v ./fannkuchredux.gcc-5.gcc_run 12
