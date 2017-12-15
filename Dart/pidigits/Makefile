compile:

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dart  pidigits.dart 2000" Dart pidigits

run:
	/usr/bin/dart  pidigits.dart 2000

mem:
	/usr/bin/time -v  /usr/bin/dart  pidigits.dart 2000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/bin/dart  pidigits.dart 2000
