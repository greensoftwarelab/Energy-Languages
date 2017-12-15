compile:
	cp mandelbrot.typescript mandelbrot.ts
	/usr/local/src/node-v7.9.0-linux-x64/bin/tsc --alwaysStrict -t ESNEXT  mandelbrot.ts

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict mandelbrot.js 16000" TypeScript mandelbrot

run:
	/usr/local/src/node-v7.9.0-linux-x64/bin/node --use_strict mandelbrot.js 16000

clean:
	rm -rf *.js *.ts
