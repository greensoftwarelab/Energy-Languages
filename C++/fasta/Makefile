-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -pthread fasta.cpp -o fasta_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta_run 25000000" C++ fasta

run:
	./fasta_run 25000000

mem:
	/usr/bin/time -v ./fasta_run 25000000

valgrind:
	valgrind --tool=massif --stacks=yes ./fasta_run 25000000
