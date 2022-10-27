ifndef NODE
NODE := node
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt" JavaScript regex-redux

run:
	$(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v $(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt
