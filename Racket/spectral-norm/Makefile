compile:
	$(RACO) make spectralnorm.racket-3.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/spectralnorm.racket-3_racket.zo 5500" Racket spectral-norm

run:
	$(RACKET) ./compiled/spectralnorm.racket-3_racket.zo 5500

mem:
	/usr/bin/time -v $(RACKET) ./compiled/spectralnorm.racket-3_racket.zo 5500

valgrind:
	valgrind --tool=massif --stacks=yes $(RACKET) ./compiled/spectralnorm.racket-3_racket.zo 5500
