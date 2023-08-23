ifndef PHP
PHP := php
endif

compile:

run:
	php -n fasta.php 25000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" php -n fasta.php 25000000
