compile:
	$(RACO) make knucleotide.racket-4.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/knucleotide.racket-4_racket.zo 0 < knucleotide-input25000000.txt" Racket k-nucleotide

run:
	$(RACKET) ./compiled/knucleotide.racket-4_racket.zo 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v $(RACKET) ./compiled/knucleotide.racket-4_racket.zo 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(RACKET) ./compiled/knucleotide.racket-4_racket.zo 0 < knucleotide-input25000000.txt
