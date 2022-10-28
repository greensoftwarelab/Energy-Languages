-include ../Makefile.defaults

compile: binarytrees.c
	$(CC) $(CFLAGS) $(shell pkg-config --cflags apr-1) -fopenmp binarytrees.c -o binarytrees_run $(shell pkg-config --libs apr-1)

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees_run 21" C binary-trees

run:
	./binarytrees_run 21

mem:
	/usr/bin/time -v ./binarytrees_run 21
