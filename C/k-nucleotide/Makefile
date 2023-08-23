-include ../Makefile.defaults

compile:
	wget https://github.com/samtools/htslib/releases/download/1.17/htslib-1.17.tar.bz2 -O - | tar xj
	$(CC) $(CFLAGS) -fopenmp -Ihtslib-1.17 knucleotide.c -o knucleotide_run

run:
	./knucleotide_run 0 < ../../knucleotide-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" ./knucleotide_run 0 < ../../knucleotide-input25000000.txt
