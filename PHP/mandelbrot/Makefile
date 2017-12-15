compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "php  mandelbrot.php-3.php 16000" PHP mandelbrot

run:
	php  mandelbrot.php-3.php 16000

mem:
	/usr/bin/time -v php  mandelbrot.php-3.php 16000

valgrind:
	valgrind --tool=massif --stacks=yes php  mandelbrot.php-3.php 16000
