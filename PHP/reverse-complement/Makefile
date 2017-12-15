compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "php -n -d memory_limit=1024M revcomp.php-3.php 0 < ../../revcomp-input25000000.txt" PHP reverse-complement

run:
	php -n -d memory_limit=1024M revcomp.php-3.php 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v php -n -d memory_limit=1024M revcomp.php-3.php 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes php -n -d memory_limit=1024M revcomp.php-3.php 0 < ../../revcomp-input25000000.txt



