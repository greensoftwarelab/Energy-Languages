compile:
	/usr/local/src/racket-6.8/bin/raco make mandelbrot.racket-3.racket

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/racket-6.8/bin/racket ./compiled/mandelbrot.racket-3_racket.zo 16000" Racket mandelbrot

run:
	/usr/local/src/racket-6.8/bin/racket ./compiled/mandelbrot.racket-3_racket.zo 16000

mem:
	/usr/bin/time -v /usr/local/src/racket-6.8/bin/racket ./compiled/mandelbrot.racket-3_racket.zo 16000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/racket-6.8/bin/racket ./compiled/mandelbrot.racket-3_racket.zo 16000
