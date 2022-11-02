compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RUBY) -W0 knucleotide.yarv-7.yarv 0 < knucleotide-input25000000.txt" Ruby k-nucleotide

run:
	$(RUBY) -W0 knucleotide.yarv-7.yarv 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v $(RUBY) -W0 knucleotide.yarv-7.yarv 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(RUBY) -W0 knucleotide.yarv-7.yarv 0 < knucleotide-input25000000.txt
