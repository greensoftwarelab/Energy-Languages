compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RUBY) -W0 spectralnorm.yarv-5.yarv 5500" Ruby spectral-norm

run:
	$(RUBY) -W0 spectralnorm.yarv-5.yarv 5500

mem:
	/usr/bin/time -v $(RUBY) -W0 spectralnorm.yarv-5.yarv 5500

valgrind:
	valgrind --tool=massif --stacks=yes $(RUBY) -W0 spectralnorm.yarv-5.yarv 5500

