compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PERL) nbody.perl-2.perl 50000000" Perl n-body

run:
	$(PERL) nbody.perl-2.perl 50000000

mem:
	/usr/bin/time -v $(PERL) nbody.perl-2.perl 50000000

valgrind:
	valgrind --tool=massif --stacks=yes $(PERL) nbody.perl-2.perl 50000000
