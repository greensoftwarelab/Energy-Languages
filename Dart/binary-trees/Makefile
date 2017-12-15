compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dart  binarytrees.dart 21" Dart binary-trees

run:
	/usr/bin/dart  binarytrees.dart 21

mem:
	/usr/bin/time -v  /usr/bin/dart  binarytrees.dart 21

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/bin/dart  binarytrees.dart 21
