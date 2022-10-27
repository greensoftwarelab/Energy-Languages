ifndef NODE
NODE := node
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt" JavaScript k-nucleotide

run:
	$(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt

mem:
	/usr/bin/time -v $(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) knucleotide.js 0 < ../../knucleotide-input25000000.txt
