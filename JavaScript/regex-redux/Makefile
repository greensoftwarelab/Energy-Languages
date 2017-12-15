compile:
	cp -L regexredux.node-2.node regexredux.node-2.js
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node regexredux.node-2.js 0 < ../../regexredux-input5000000.txt" JavaScript regex-redux

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node regexredux.node-2.js 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node regexredux.node-2.js 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node regexredux.node-2.js 0 < ../../regexredux-input5000000.txt
