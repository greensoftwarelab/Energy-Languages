compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PERL) knucleotide.perl 0 < ../../knucleotide-input25000000.txt" Perl k-nucleotide

run:
	$(PERL) knucleotide.perl 0 < ../../knucleotide-input25000000.txt

mem:
	/usr/bin/time -v $(PERL) knucleotide.perl 0 < ../../knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(PERL) knucleotide.perl 0 < ../../knucleotide-input25000000.txt
