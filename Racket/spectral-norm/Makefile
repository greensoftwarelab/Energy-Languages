compile:
	/usr/local/src/racket-6.8/bin/raco make spectralnorm.racket-3.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/racket-6.8/bin/racket ./compiled/spectralnorm.racket-3_racket.zo 5500" Racket spectral-norm

run:
	/usr/local/src/racket-6.8/bin/racket ./compiled/spectralnorm.racket-3_racket.zo 5500

mem:
	/usr/bin/time -v /usr/local/src/racket-6.8/bin/racket ./compiled/spectralnorm.racket-3_racket.zo 5500

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/racket-6.8/bin/racket ./compiled/spectralnorm.racket-3_racket.zo 5500
