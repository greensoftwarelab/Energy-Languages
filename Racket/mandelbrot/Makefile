compile:
	$(RACO) make mandelbrot.racket-3.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RACKET) ./compiled/mandelbrot.racket-3_racket.zo 16000" Racket mandelbrot

run:
	$(RACKET) ./compiled/mandelbrot.racket-3_racket.zo 16000

mem:
	/usr/bin/time -v $(RACKET) ./compiled/mandelbrot.racket-3_racket.zo 16000

valgrind:
	valgrind --tool=massif --stacks=yes $(RACKET) ./compiled/mandelbrot.racket-3_racket.zo 16000
