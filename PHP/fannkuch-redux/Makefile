ifndef PHP
PHP := php
endif

compile:

run:
	php fannkuchredux.php 12

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" php fannkuchredux.php 12

