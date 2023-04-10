-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -fopenmp regexredux.c -o regexredux_run -lpcre

run:
	./regexredux_run 0 < ../../regexredux-input5000000.txt

measure:
ifndef CSV
	$(error CSV is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(CSV) './regexredux_run 0' < ../../regexredux-input5000000.txt
