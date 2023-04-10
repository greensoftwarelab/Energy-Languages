-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -fopenmp fannkuchredux.c -o fannkuchredux_run

run:
	./fannkuchredux_run 12

measure:
ifndef CSV
	$(error CSV is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(CSV) './fannkuchredux_run 12'
