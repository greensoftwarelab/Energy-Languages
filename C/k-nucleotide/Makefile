compile:
	/usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -fopenmp -std=c99 -IInclude knucleotide.c -o knucleotide.gcc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./knucleotide.gcc_run 0 < knucleotide-input25000000.txt" C k-nucleotide

run:
	./knucleotide.gcc_run 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v ./knucleotide.gcc_run 0 < knucleotide-input25000000.txt
