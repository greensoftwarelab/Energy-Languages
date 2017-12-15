compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/perl-5.24.0/bin/perl fannkuchredux.perl-2.perl 12" Perl fannkuch-redux

run:
	/usr/local/src/perl-5.24.0/bin/perl fannkuchredux.perl-2.perl 12

mem:
	/usr/bin/time -v /usr/local/src/perl-5.24.0/bin/perl fannkuchredux.perl-2.perl 12

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/perl-5.24.0/bin/perl fannkuchredux.perl-2.perl 12
