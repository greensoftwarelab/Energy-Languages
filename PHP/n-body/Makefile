compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "php -n  nbody.php-3.php 50000000" PHP n-body

run:
	php -n  nbody.php-3.php 50000000

mem:
	/usr/bin/time -v  php -n  nbody.php-3.php 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  php -n  nbody.php-3.php 50000000
