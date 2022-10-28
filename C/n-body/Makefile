-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -mfpmath=sse -msse3 nbody.c -o nbody_run -lm

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody_run 50000000" C n-body

run:
	./nbody_run 50000000

mem:
	/usr/bin/time -v ./nbody_run 50000000
