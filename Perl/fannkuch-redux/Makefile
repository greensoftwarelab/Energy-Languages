compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PERL) fannkuchredux.perl-2.perl 12" Perl fannkuch-redux

run:
	$(PERL) fannkuchredux.perl-2.perl 12

mem:
	/usr/bin/time -v $(PERL) fannkuchredux.perl-2.perl 12

valgrind:
	valgrind --tool=massif --stacks=yes $(PERL) fannkuchredux.perl-2.perl 12
