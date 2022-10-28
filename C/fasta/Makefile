-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -mfpmath=sse -msse3 -fopenmp fasta.c -o fasta_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta_run 25000000" C fasta

run:
	./fasta_run 25000000

mem:
	/usr/bin/time -v ./fasta_run 25000000
