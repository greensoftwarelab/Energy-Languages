compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PERL) binarytrees.perl-3.perl 21" Perl binary-trees

run:
	$(PERL) binarytrees.perl-3.perl 21

mem:
	/usr/bin/time -v  $(PERL) binarytrees.perl-3.perl 21

valgrind:
	valgrind --tool=massif --stacks=yes  $(PERL) binarytrees.perl-3.perl 21
