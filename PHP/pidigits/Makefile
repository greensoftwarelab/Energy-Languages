compile:

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "php -n  pidigits.php-5.php 10000" PHP pidigits

run:
	php -n  pidigits.php-5.php 10000

mem:
	/usr/bin/time -v php -n  pidigits.php-5.php 10000

valgrind:
	valgrind --tool=massif --stacks=yes php -n  pidigits.php-5.php 10000
