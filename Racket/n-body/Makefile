compile:
	$(RACO) make nbody.racket-2.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/nbody.racket-2_racket.zo 50000000" Racket n-body

run:
	$(RACKET) ./compiled/nbody.racket-2_racket.zo 50000000

mem:
	/usr/bin/time -v  $(RACKET) ./compiled/nbody.racket-2_racket.zo 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  $(RACKET) ./compiled/nbody.racket-2_racket.zo 50000000
