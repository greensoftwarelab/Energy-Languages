compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PERL) regexredux.perl-4.perl 0 < ../../regexredux-input5000000.txt" Perl regex-redux

run:
	$(PERL) regexredux.perl-4.perl 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v $(PERL) regexredux.perl-4.perl 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(PERL) regexredux.perl-4.perl 0 < ../../regexredux-input5000000.txt
