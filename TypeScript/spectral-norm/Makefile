ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	$(TSC) --alwaysStrict -t ESNEXT spectralnorm.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) spectralnorm.js 5500" TypeScript spectral-norm

run:
	$(NODE) spectralnorm.js 5500

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v $(NODE) spectralnorm.js 5500

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) spectralnorm.js 5500
