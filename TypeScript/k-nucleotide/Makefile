ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	$(TSC) --alwaysStrict -t ESNEXT knucleotide.ts

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt" TypeScript k-nucleotide

run:
	$(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v $(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt
