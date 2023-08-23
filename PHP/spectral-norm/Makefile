ifndef PHP
PHP := php
endif

compile:

run:
	php -n spectralnorm.php 5500

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" php -n spectralnorm.php 5500
