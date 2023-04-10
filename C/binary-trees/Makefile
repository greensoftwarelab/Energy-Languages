-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) $(shell pkg-config --cflags apr-1) -fopenmp binarytrees.c -o binarytrees_run $(shell pkg-config --libs apr-1)

run:
	./binarytrees_run 21

measure:
ifndef CSV
	$(error CSV is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(CSV) './binarytrees_run 21'
