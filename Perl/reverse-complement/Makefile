compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PERL) revcomp.perl-3.perl 0 < ../../revcomp-input25000000.txt" Perl reverse-complement

run:
	$(PERL) revcomp.perl-3.perl 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(PERL) revcomp.perl-3.perl 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(PERL) revcomp.perl-3.perl 0 < ../../revcomp-input25000000.txt
