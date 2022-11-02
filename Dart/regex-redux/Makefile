compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DART)  regexredux.dart-2.dart 0 < ../../regexredux-input5000000.txt" Dart regex-redux

run:
	$(DART)  regexredux.dart-2.dart 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v  $(DART)  regexredux.dart-2.dart 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  $(DART)  regexredux.dart-2.dart 0 < ../../regexredux-input5000000.txt
