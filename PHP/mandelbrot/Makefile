ifndef PHP
PHP := php
endif

compile:

run:
	php mandelbrot.php 16000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" php mandelbrot.php 16000
