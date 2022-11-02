compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DART)  nbody.dart-3.dart 50000000" Dart n-body

run:
	$(DART)  nbody.dart-3.dart 50000000

mem:
	/usr/bin/time -v $(DART)  nbody.dart-3.dart 50000000

valgrind:
	valgrind --tool=massif --stacks=yes $(DART)  nbody.dart-3.dart 50000000
