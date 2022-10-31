compile:
	$(RACO) make revcomp.racket-2.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/revcomp.racket-2_racket.zo 0 < ../../revcomp-input25000000.txt" Racket reverse-complement

run:
	$(RACKET) ./compiled/revcomp.racket-2_racket.zo 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(RACKET) ./compiled/revcomp.racket-2_racket.zo 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(RACKET) ./compiled/revcomp.racket-2_racket.zo 0 < ../../revcomp-input25000000.txt
