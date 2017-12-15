compile:
	/usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -mfpmath=sse -msse3 nbody.gcc-4.c -o nbody.gcc-4.gcc_run -lm
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody.gcc-4.gcc_run 50000000" C n-body

run:
	./nbody.gcc-4.gcc_run 50000000

mem:
	/usr/bin/time -v ./nbody.gcc-4.gcc_run 50000000
