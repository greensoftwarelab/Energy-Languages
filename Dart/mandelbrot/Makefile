compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dart  mandelbrot.dart 16000" Dart mandelbrot

run:
	/usr/bin/dart  mandelbrot.dart 16000

mem:
	/usr/bin/time -v /usr/bin/dart  mandelbrot.dart 16000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/bin/dart  mandelbrot.dart 16000
