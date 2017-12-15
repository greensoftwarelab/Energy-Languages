compile:
	cp -L fannkuchredux.node-4.node fannkuchredux.node-4.js
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node fannkuchredux.node-4.js 12" JavaScript fannkuch-redux

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node fannkuchredux.node-4.js 12

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node fannkuchredux.node-4.js 12

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node fannkuchredux.node-4.js 12
