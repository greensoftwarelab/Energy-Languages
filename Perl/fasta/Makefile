compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PERL) fasta.perl 25000000" Perl fasta

run:
	$(PERL) fasta.perl 25000000
mem:
	/usr/bin/time -v $(PERL) fasta.perl 25000000

valgrind:
	valgrind --tool=massif --stacks=yes $(PERL) fasta.perl 25000000

