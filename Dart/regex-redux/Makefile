compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dart  regexredux.dart-2.dart 0 < ../../regexredux-input5000000.txt" Dart regex-redux

run:
	/usr/bin/dart  regexredux.dart-2.dart 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v  /usr/bin/dart  regexredux.dart-2.dart 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/bin/dart  regexredux.dart-2.dart 0 < ../../regexredux-input5000000.txt
