-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -fopenmp regexredux.c -o regexredux_run -lpcre

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./regexredux_run 0 < ../../regexredux-input5000000.txt" C regex-redux

run:
	./regexredux_run 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v ./regexredux_run 0 < ../../regexredux-input5000000.txt
