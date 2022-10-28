-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -funroll-loops -fopenmp revcomp.c -o revcomp_run 

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp_run 0 < ../../revcomp-input25000000.txt" C reverse-complement

run:
	./revcomp_run 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v ./revcomp_run 0 < ../../revcomp-input25000000.txt
