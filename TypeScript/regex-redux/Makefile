ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	npm install
	$(TSC) --alwaysStrict -t ESNEXT regexredux.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt" TypeScript regex-redux

run:
	$(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v $(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) regexredux.js 0 < ../../regexredux-input5000000.txt
