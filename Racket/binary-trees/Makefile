compile:
	/usr/local/src/racket-6.8/bin/raco make binarytrees.racket-3.racket
	
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/racket-6.8/bin/racket ./compiled/binarytrees.racket-3_racket.zo 21" Racket binary-trees

run:
	/usr/local/src/racket-6.8/bin/racket ./compiled/binarytrees.racket-3_racket.zo 21

mem:
	/usr/bin/time -v  /usr/local/src/racket-6.8/bin/racket ./compiled/binarytrees.racket-3_racket.zo 21

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/racket-6.8/bin/racket ./compiled/binarytrees.racket-3_racket.zo 21
