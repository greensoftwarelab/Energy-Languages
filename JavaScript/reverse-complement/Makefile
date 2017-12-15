compile:
	cp -L revcomp.node-7.node revcomp.node-7.js
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node revcomp.node-7.js 0 < ../../revcomp-input25000000.txt" JavaScript reverse-complement

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node revcomp.node-7.js 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node revcomp.node-7.js 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node revcomp.node-7.js 0 < ../../revcomp-input25000000.txt
