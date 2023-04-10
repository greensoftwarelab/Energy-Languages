-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -mfpmath=sse -msse3 -fopenmp fasta.c -o fasta_run

run:
	./fasta_run 25000000

measure:
ifndef CSV
	$(error CSV is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(CSV) './fasta_run 25000000'
