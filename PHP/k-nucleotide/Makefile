ifndef PHP
PHP := php
endif

compile:

run:
	php -d memory_limit=1024M knucleotide.php 0 < ../../knucleotide-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" php -d memory_limit=1024M knucleotide.php 0 < ../../knucleotide-input25000000.txt
