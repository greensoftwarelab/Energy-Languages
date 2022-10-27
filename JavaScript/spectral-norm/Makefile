ifndef NODE
NODE := node
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) spectralnorm.js 5500" JavaScript spectral-norm

run:
	$(NODE) spectralnorm.js 5500

mem:
	/usr/bin/time -v $(NODE) spectralnorm.js 5500

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) spectralnorm.js 5500
