compile:
	/usr/local/src/racket-6.8/bin/raco make revcomp.racket-2.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/racket-6.8/bin/racket ./compiled/revcomp.racket-2_racket.zo 0 < ../../revcomp-input25000000.txt" Racket reverse-complement

run:
	/usr/local/src/racket-6.8/bin/racket ./compiled/revcomp.racket-2_racket.zo 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/racket-6.8/bin/racket ./compiled/revcomp.racket-2_racket.zo 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/racket-6.8/bin/racket ./compiled/revcomp.racket-2_racket.zo 0 < ../../revcomp-input25000000.txt
