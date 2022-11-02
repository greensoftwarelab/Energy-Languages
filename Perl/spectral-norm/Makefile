compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PERL) spectralnorm.perl-4.perl 5500" Perl spectral-norm

run:
	$(PERL) spectralnorm.perl-4.perl 5500

mem:
	/usr/bin/time -v  $(PERL) spectralnorm.perl-4.perl 5500

valgrind:
	valgrind --tool=massif --stacks=yes  $(PERL) spectralnorm.perl-4.perl 5500
