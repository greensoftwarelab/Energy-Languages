ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	$(TSC) --alwaysStrict -t ESNEXT fasta.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) fasta.js 25000000" TypeScript fasta

run:
	$(NODE) fasta.js 25000000

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v $(NODE) fasta.js 25000000

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) fasta.js 25000000
