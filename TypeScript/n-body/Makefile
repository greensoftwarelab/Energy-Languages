ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	$(TSC) --alwaysStrict -t ESNEXT nbody.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) nbody.js 50000000" TypeScript n-body

run:
	$(NODE) nbody.js 50000000

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v $(NODE) nbody.js 50000000

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) nbody.js 50000000
