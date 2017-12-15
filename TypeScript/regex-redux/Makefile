compile:
	cp regexredux.typescript-3.typescript regexredux.typescript-3.ts
	/usr/local/src/node-v7.9.0-linux-x64/bin/tsc --alwaysStrict -t ESNEXT --lib es7 regexredux.typescript-3.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict regexredux.typescript-3.js 0 < ../../regexredux-input5000000.txt" TypeScript regex-redux

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict regexredux.typescript-3.js 0 < ../../regexredux-input5000000.txt

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict regexredux.typescript-3.js 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict regexredux.typescript-3.js 0 < ../../regexredux-input5000000.txt
