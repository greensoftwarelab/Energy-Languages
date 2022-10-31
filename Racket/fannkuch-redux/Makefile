compile:
	$(RACO) make fannkuchredux.racket-3.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/fannkuchredux.racket-3_racket.zo 12" Racket fannkuch-redux

run:
	$(RACKET) ./compiled/fannkuchredux.racket-3_racket.zo 12

mem:
	/usr/bin/time -v $(RACKET) ./compiled/fannkuchredux.racket-3_racket.zo 12

valgrind:
	valgrind --tool=massif --stacks=yes $(RACKET) ./compiled/fannkuchredux.racket-3_racket.zo 12
