compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DART)  binarytrees.dart 21" Dart binary-trees

run:
	$(DART)  binarytrees.dart 21

mem:
	/usr/bin/time -v  $(DART)  binarytrees.dart 21

valgrind:
	valgrind --tool=massif --stacks=yes  $(DART)  binarytrees.dart 21
