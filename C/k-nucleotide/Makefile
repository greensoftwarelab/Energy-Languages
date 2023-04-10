-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -fopenmp -std=c99 knucleotide.c -o knucleotide.gcc_run

run:
	./knucleotide.gcc_run 0 < ../../knucleotide-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) './knucleotide.gcc_run 0' < ../../knucleotide-input25000000.txt
