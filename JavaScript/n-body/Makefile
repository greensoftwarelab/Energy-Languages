compile:
	cp -L nbody.node nbody.js
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node nbody.js 50000000" JavaScript n-body

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node nbody.js 50000000

mem:
	/usr/bin/time -v  /usr/local/src/node-v7.9.0-linux-x64/bin/node nbody.js 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/node-v7.9.0-linux-x64/bin/node nbody.js 50000000
