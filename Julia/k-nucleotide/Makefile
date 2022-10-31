compile:
	
measure:
	export JULIA_NUM_THREADS=4
	sudo ../../RAPL/main "$(JULIA) -O3  -- knucleotide.julia-7.julia 0 < ../../knucleotide-input25000000.txt" Julia k-nucleotide
run:
	export JULIA_NUM_THREADS=4
	$(JULIA) -O3  -- knucleotide.julia-7.julia 0 < ../../knucleotide-input25000000.txt
mem:
	export JULIA_NUM_THREADS=4
	/usr/bin/time -v  $(JULIA) -O3  -- knucleotide.julia-7.julia 0 < ../../knucleotide-input25000000.txt
valmem:
	export JULIA_NUM_THREADS=4
	valgrind --tool=massif --stacks=yes  $(JULIA) -O3  -- knucleotide.julia-7.julia 0 < ../../knucleotide-input25000000.txt
