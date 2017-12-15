compile:
	/usr/local/src/racket-6.8/bin/raco make nbody.racket-2.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/racket-6.8/bin/racket ./compiled/nbody.racket-2_racket.zo 50000000" Racket n-body

run:
	/usr/local/src/racket-6.8/bin/racket ./compiled/nbody.racket-2_racket.zo 50000000

mem:
	/usr/bin/time -v  /usr/local/src/racket-6.8/bin/racket ./compiled/nbody.racket-2_racket.zo 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/racket-6.8/bin/racket ./compiled/nbody.racket-2_racket.zo 50000000
