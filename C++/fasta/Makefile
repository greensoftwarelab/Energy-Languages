compile:
	/usr/bin/g++ -c -pipe -O3 -fomit-frame-pointer -march=native -mfpmath=sse -msse3 -std=c++11 fasta.gpp-5.c++ -o fasta.gpp-5.c++.o &&  /usr/bin/g++ fasta.gpp-5.c++.o -o fasta.gpp-5.gpp_run -lpthread 

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta.gpp-5.gpp_run 25000000" C++ fasta

run:
	./fasta.gpp-5.gpp_run 25000000

mem:
	/usr/bin/time -v ./fasta.gpp-5.gpp_run 25000000

valgrind:
	valgrind --tool=massif --stacks=yes ./fasta.gpp-5.gpp_run 25000000

