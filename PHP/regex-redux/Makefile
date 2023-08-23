ifndef PHP
PHP := php
endif

compile:

run:
	php -d memory_limit=512M regexredux.php 0 < ../../regexredux-input5000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" php -d memory_limit=512M regexredux.php 0 < ../../regexredux-input5000000.txt
