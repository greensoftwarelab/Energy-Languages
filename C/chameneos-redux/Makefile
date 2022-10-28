-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -pthread chameneosredux.c -o chameneosredux_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./chameneosredux_run 6000000"

run:
	./chameneosredux_run 6000000

mem:
	/usr/bin/time -v ./chameneosredux_run 6000000
