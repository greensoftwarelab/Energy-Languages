ifndef NODE
NODE := node
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) mandelbrot.js 16000" JavaScript mandelbrot

run:
	$(NODE) mandelbrot.js 16000

mem:
	/usr/bin/time -v $(NODE) mandelbrot.js 16000

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) mandelbrot.js 16000
