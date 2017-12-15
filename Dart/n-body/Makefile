compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dart  nbody.dart-3.dart 50000000" Dart n-body

run:
	/usr/bin/dart  nbody.dart-3.dart 50000000

mem:
	/usr/bin/time -v /usr/bin/dart  nbody.dart-3.dart 50000000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/bin/dart  nbody.dart-3.dart 50000000
