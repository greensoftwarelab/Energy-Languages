compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/perl-5.24.0/bin/perl regexredux.perl-4.perl 0 < ../../regexredux-input5000000.txt" Perl regex-redux

run:
	/usr/local/src/perl-5.24.0/bin/perl regexredux.perl-4.perl 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v /usr/local/src/perl-5.24.0/bin/perl regexredux.perl-4.perl 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/perl-5.24.0/bin/perl regexredux.perl-4.perl 0 < ../../regexredux-input5000000.txt
