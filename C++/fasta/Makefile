-include ../Makefile.defaults

compile:
	$(CXX) $(CXXFLAGS) -pthread fasta.cpp -o fasta_run

run:
	./fasta_run 25000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(JSON) './fasta_run 25000000'
