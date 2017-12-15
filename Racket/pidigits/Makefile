compile:
	/usr/local/src/racket-6.8/bin/raco make pidigits.racket-2.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/racket-6.8/bin/racket ./compiled/pidigits.racket-2_racket.zo 10000" Racket pidigits

run:
	/usr/local/src/racket-6.8/bin/racket ./compiled/pidigits.racket-2_racket.zo 10000

mem:
	/usr/bin/time -v /usr/local/src/racket-6.8/bin/racket ./compiled/pidigits.racket-2_racket.zo 10000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/racket-6.8/bin/racket ./compiled/pidigits.racket-2_racket.zo 10000
