compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DART)  spectralnorm.dart 5500" Dart spectral-norm

run:
	$(DART)  spectralnorm.dart 5500

mem:
	/usr/bin/time -v $(DART)  spectralnorm.dart 5500

valgrind:
	valgrind --tool=massif --stacks=yes $(DART)  spectralnorm.dart 5500
