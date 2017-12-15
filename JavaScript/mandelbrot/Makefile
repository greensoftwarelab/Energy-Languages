compile:
	cp -L mandelbrot.node mandelbrot.js
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node mandelbrot.js 16000" JavaScript mandelbrot

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node mandelbrot.js 16000

mem:
	/usr/bin/time -v /usr/local/src/node-v7.9.0-linux-x64/bin/node mandelbrot.js 16000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/node-v7.9.0-linux-x64/bin/node mandelbrot.js 16000
