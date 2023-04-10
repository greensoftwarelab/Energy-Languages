-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -fopenmp revcomp.c -o revcomp_run

run:
	./revcomp_run 0 < ../../revcomp-input25000000.txt

measure:
ifndef CSV
	$(error CSV is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(CSV) './revcomp_run 0' < ../../revcomp-input25000000.txt
