ifndef TSC
TSC := tsc
endif

ifndef NODE
NODE := node
endif

compile:
	$(TSC) --alwaysStrict -t ESNEXT revcomp.ts
	
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt" TypeScript reverse-complement

run:
	$(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt

clean:
	rm -rf *.js *.ts

mem:
	/usr/bin/time -v $(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt
