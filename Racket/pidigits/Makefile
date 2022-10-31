compile:
	$(RACO) make pidigits.racket-2.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/pidigits.racket-2_racket.zo 10000" Racket pidigits

run:
	$(RACKET) ./compiled/pidigits.racket-2_racket.zo 10000

mem:
	/usr/bin/time -v $(RACKET) ./compiled/pidigits.racket-2_racket.zo 10000

valgrind:
	valgrind --tool=massif --stacks=yes  $(RACKET) ./compiled/pidigits.racket-2_racket.zo 10000
