compile:
	cp -L spectralnorm.node-2.node spectralnorm.node-2.js
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node spectralnorm.node-2.js 5500" JavaScript spectral-norm

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node spectralnorm.node-2.js 5500

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node spectralnorm.node-2.js 5500

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node spectralnorm.node-2.js 5500
