ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	npm install
	$(TSC) --alwaysStrict -t ESNEXT fannkuchredux.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) fannkuchredux.js 12" TypeScript fannkuch-redux

run:
	$(NODE) fannkuchredux.js 12

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v $(NODE) fannkuchredux.js 12

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) fannkuchredux.js 12
