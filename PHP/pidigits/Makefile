ifndef PHP
PHP := php
endif

compile:

run:
	php -c . pidigits.php 10000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" php -c . pidigits.php 10000
