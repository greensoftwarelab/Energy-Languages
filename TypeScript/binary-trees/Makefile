ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	npm install
	$(TSC) --alwaysStrict -t ESNEXT binarytrees.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) binarytrees.js 21" TypeScript binary-trees

run:
	$(NODE) binarytrees.js 21

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v $(NODE) binarytrees.js 21

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) binarytrees.js 21
