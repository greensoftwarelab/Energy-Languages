ifndef PHP
PHP := php
endif

compile:

run:
	php -n -d memory_limit=4096M binarytrees.php 21

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" php -n -d memory_limit=4096M binarytrees.php 21
