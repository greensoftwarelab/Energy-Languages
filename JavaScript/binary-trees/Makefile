compile:
	cp -L binarytrees.node binarytrees.js
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node binarytrees.js 21" JavaScript binary-trees

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node binarytrees.js 21

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node binarytrees.js 21

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node binarytrees.js 21
