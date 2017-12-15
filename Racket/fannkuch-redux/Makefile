compile:
	/usr/local/src/racket-6.8/bin/raco make fannkuchredux.racket-3.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/racket-6.8/bin/racket ./compiled/fannkuchredux.racket-3_racket.zo 12" Racket fannkuch-redux

run:
	/usr/local/src/racket-6.8/bin/racket ./compiled/fannkuchredux.racket-3_racket.zo 12

mem:
	/usr/bin/time -v /usr/local/src/racket-6.8/bin/racket ./compiled/fannkuchredux.racket-3_racket.zo 12

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/racket-6.8/bin/racket ./compiled/fannkuchredux.racket-3_racket.zo 12
