compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DART)  mandelbrot.dart 16000" Dart mandelbrot

run:
	$(DART)  mandelbrot.dart 16000

mem:
	/usr/bin/time -v $(DART)  mandelbrot.dart 16000

valgrind:
	valgrind --tool=massif --stacks=yes $(DART)  mandelbrot.dart 16000
