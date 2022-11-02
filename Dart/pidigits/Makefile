compile:

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DART)  pidigits.dart 2000" Dart pidigits

run:
	$(DART)  pidigits.dart 2000

mem:
	/usr/bin/time -v  $(DART)  pidigits.dart 2000

valgrind:
	valgrind --tool=massif --stacks=yes  $(DART)  pidigits.dart 2000
