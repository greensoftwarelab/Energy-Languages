compile:
	cp -L knucleotide.node-2.node knucleotide.node-2.js
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node knucleotide.node-2.js 0 < knucleotide-input25000000.txt" JavaScript k-nucleotide

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node knucleotide.node-2.js 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node knucleotide.node-2.js 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node knucleotide.node-2.js 0 < knucleotide-input25000000.txt
