compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "php -n  fasta.php-3.php 25000000" PHP fasta

run:
	php -n  fasta.php-3.php 25000000

mem:
	/usr/bin/time -v php -n  fasta.php-3.php 25000000

valgrind:
	valgrind --tool=massif --stacks=yes php -n  fasta.php-3.php 25000000
