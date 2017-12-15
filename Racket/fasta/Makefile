compile:
	/usr/local/src/racket-6.8/bin/raco make fasta.racket-3.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/racket-6.8/bin/racket ./compiled/fasta.racket-3_racket.zo 25000000" Racket fasta

run:
	/usr/local/src/racket-6.8/bin/racket ./compiled/fasta.racket-3_racket.zo 25000000

mem:
	/usr/bin/time -v /usr/local/src/racket-6.8/bin/racket ./compiled/fasta.racket-3_racket.zo 25000000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/racket-6.8/bin/racket ./compiled/fasta.racket-3_racket.zo 25000000
