compile:
	cp spectralnorm.typescript spectralnorm.ts
	/usr/local/src/node-v7.9.0-linux-x64/bin/tsc --alwaysStrict -t ESNEXT  spectralnorm.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict spectralnorm.js 5500" TypeScript spectral-norm

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict spectralnorm.js 5500

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict spectralnorm.js 5500

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict spectralnorm.js 5500

