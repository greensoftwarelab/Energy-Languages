ifndef NODE
NODE := node
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt" JavaScript reverse-complement

run:
	$(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(NODE) revcomp.js 0 < ../../revcomp-input25000000.txt
