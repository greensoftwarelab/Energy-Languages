compile:
	/usr/local/src/racket-6.8/bin/raco make knucleotide.racket-4.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/racket-6.8/bin/racket ./compiled/knucleotide.racket-4_racket.zo 0 < knucleotide-input25000000.txt" Racket k-nucleotide

run:
	/usr/local/src/racket-6.8/bin/racket ./compiled/knucleotide.racket-4_racket.zo 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/racket-6.8/bin/racket ./compiled/knucleotide.racket-4_racket.zo 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/racket-6.8/bin/racket ./compiled/knucleotide.racket-4_racket.zo 0 < knucleotide-input25000000.txt
