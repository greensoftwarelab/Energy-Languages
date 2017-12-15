compile:
	cp nbody.typescript-3.typescript nbody.typescript-3.ts
	/usr/local/src/node-v7.9.0-linux-x64/bin/tsc --alwaysStrict -t ESNEXT  nbody.typescript-3.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict nbody.typescript-3.js 50000000" TypeScript n-body

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict nbody.typescript-3.js 50000000

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict nbody.typescript-3.js 50000000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict nbody.typescript-3.js 50000000
