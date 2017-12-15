compile:
	cp -L fasta.node-4.node fasta.node-4.js
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node fasta.node-4.js 25000000" JavaScript fasta

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node fasta.node-4.js 25000000

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node fasta.node-4.js 25000000


valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node fasta.node-4.js 25000000

