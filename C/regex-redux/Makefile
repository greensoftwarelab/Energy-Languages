-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -fopenmp regexredux.c -o regexredux_run -lpcre

run:
	./regexredux_run 0 < ../../regexredux-input5000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./regexredux_run 0 < ../../regexredux-input5000000.txt
