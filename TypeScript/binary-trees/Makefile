compile:
	cp binarytrees.typescript-2.typescript binarytrees.typescript-2.ts
	/usr/local/src/node-v7.9.0-linux-x64/bin/tsc --alwaysStrict -t ESNEXT  binarytrees.typescript-2.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict binarytrees.typescript-2.js 21" TypeScript binary-trees

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict binarytrees.typescript-2.js 21

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict binarytrees.typescript-2.js 21

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict binarytrees.typescript-2.js 21

