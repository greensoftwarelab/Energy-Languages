compile:
	cp revcomp.typescript revcomp.ts
	/usr/local/src/node-v7.9.0-linux-x64/bin/tsc --alwaysStrict -t ESNEXT  revcomp.ts
	
xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict revcomp.js 0 < ../../revcomp-input25000000.txt" TypeScript reverse-complement

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict revcomp.js 0 < ../../revcomp-input25000000.txt

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict revcomp.js 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict revcomp.js 0 < ../../revcomp-input25000000.txt
