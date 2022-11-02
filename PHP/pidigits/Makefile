compile:

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "php -c . pidigits.php-5.php 10000" PHP pidigits

run:
	php -c . pidigits.php-5.php 10000

mem:
	/usr/bin/time -v php -c . pidigits.php-5.php 10000

valgrind:
	valgrind --tool=massif --stacks=yes php -c . pidigits.php-5.php 10000
