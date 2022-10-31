compile:
	$(RACO) make binarytrees.racket-3.racket
	
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/binarytrees.racket-3_racket.zo 21" Racket binary-trees

run:
	$(RACKET) ./compiled/binarytrees.racket-3_racket.zo 21

mem:
	/usr/bin/time -v  $(RACKET) ./compiled/binarytrees.racket-3_racket.zo 21

valgrind:
	valgrind --tool=massif --stacks=yes  $(RACKET) ./compiled/binarytrees.racket-3_racket.zo 21
