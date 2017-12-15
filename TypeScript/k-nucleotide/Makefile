compile:
	cp knucleotide.typescript knucleotide.ts
	/usr/local/src/node-v7.9.0-linux-x64/bin/tsc --alwaysStrict -t ESNEXT  knucleotide.ts
	/usr/local/src/node-v7.9.0-linux-x64/lib/node_modules/babel-cli/bin/babel.js --plugins transform-es2015-modules-commonjs knucleotide.js -o knucleotide.js

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict knucleotide.js 0 < knucleotide-input25000000.txt" TypeScript k-nucleotide

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict knucleotide.js 0 < knucleotide-input25000000.txt

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict knucleotide.js 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict knucleotide.js 0 < knucleotide-input25000000.txt
