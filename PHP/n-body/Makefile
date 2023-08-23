ifndef PHP
PHP := php
endif

compile:

run:
	php -n nbody.php 50000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" php -n nbody.php 50000000
