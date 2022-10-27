ifndef NODE
NODE := node
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) fasta.js 25000000" JavaScript fasta

run:
	$(NODE) fasta.js 25000000

mem:
	/usr/bin/time -v $(NODE) fasta.js 25000000

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) fasta.js 25000000
