compile:
	/usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -funroll-loops -fopenmp revcomp.gcc-6.c -o revcomp.gcc-6.gcc_run 
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.gcc-6.gcc_run 0 < ../../revcomp-input25000000.txt" C reverse-complement

run:
	./revcomp.gcc-6.gcc_run 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v ./revcomp.gcc-6.gcc_run 0 < ../../revcomp-input25000000.txt
