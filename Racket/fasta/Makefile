compile:
	$(RACO) make fasta.racket-3.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/fasta.racket-3_racket.zo 25000000" Racket fasta

run:
	$(RACKET) ./compiled/fasta.racket-3_racket.zo 25000000

mem:
	/usr/bin/time -v $(RACKET) ./compiled/fasta.racket-3_racket.zo 25000000

valgrind:
	valgrind --tool=massif --stacks=yes $(RACKET) ./compiled/fasta.racket-3_racket.zo 25000000
