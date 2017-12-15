compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "php -d memory_limit=512M regexredux.php 0 < ../../regexredux-input5000000.txt" PHP regex-redux

run:
	php -d memory_limit=512M regexredux.php 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v php -d memory_limit=512M regexredux.php 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes php -d memory_limit=512M regexredux.php 0 < ../../regexredux-input5000000.txt
