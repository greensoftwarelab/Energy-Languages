ifndef NODE
NODE := node
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) nbody.js 50000000" JavaScript n-body

run:
	$(NODE) nbody.js 50000000

mem:
	/usr/bin/time -v  $(NODE) nbody.js 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  $(NODE) nbody.js 50000000
