compile:
	/usr/bin/g++ -c -pipe -O3 -fomit-frame-pointer -march=native  -std=c++14 knucleotide.gpp-3.c++ -o knucleotide.gpp-3.c++.o &&  /usr/bin/g++ knucleotide.gpp-3.c++.o -o knucleotide.gpp-3.gpp_run -Wl,--no-as-needed -lpthread 
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./knucleotide.gpp-3.gpp_run 0 < knucleotide-input25000000.txt" C++ k-nucleotide

run:
	./knucleotide.gpp-3.gpp_run 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v ./knucleotide.gpp-3.gpp_run 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./knucleotide.gpp-3.gpp_run 0 < knucleotide-input25000000.txt
