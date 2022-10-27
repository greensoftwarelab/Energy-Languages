ifndef NODE
NODE := node
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) fannkuchredux.js 12" JavaScript fannkuch-redux

run:
	$(NODE) fannkuchredux.js 12

mem:
	/usr/bin/time -v $(NODE) fannkuchredux.js 12

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) fannkuchredux.js 12
