compile:
	/usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -std=c99 -mfpmath=sse -msse3 -fopenmp fasta.gcc-2.c -o fasta.gcc-2.gcc_run
	
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta.gcc-2.gcc_run 25000000" C fasta

run:
	./fasta.gcc-2.gcc_run 25000000

mem:
	/usr/bin/time -v ./fasta.gcc-2.gcc_run 25000000
