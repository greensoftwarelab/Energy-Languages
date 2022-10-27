ifndef NODE
NODE := node
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) binarytrees.js 21" JavaScript binary-trees

run:
	$(NODE) binarytrees.js 21

mem:
	/usr/bin/time -v $(NODE) binarytrees.js 21

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) binarytrees.js 21
