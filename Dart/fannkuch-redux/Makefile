compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DART)  fannkuchredux.dart-2.dart 12" Dart fannkuch-redux

run:
	$(DART)  fannkuchredux.dart-2.dart 12

mem:
	/usr/bin/time -v $(DART)  fannkuchredux.dart-2.dart 12

valgrind:
	valgrind --tool=massif --stacks=yes $(DART)  fannkuchredux.dart-2.dart 12
