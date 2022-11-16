ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	$(TSC) --alwaysStrict -t ESNEXT mandelbrot.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) mandelbrot.js 16000" TypeScript mandelbrot

run:
	$(NODE) mandelbrot.js 16000

clean:
	rm -rf *.js *.ts
