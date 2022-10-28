-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -fopenmp fannkuchredux.c -o fannkuchredux_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux_run 12" C fannkuch-redux

run:
	./fannkuchredux_run 12

mem:
	 /usr/bin/time -v ./fannkuchredux_run 12
