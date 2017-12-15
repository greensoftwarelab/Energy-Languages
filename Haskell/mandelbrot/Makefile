compile:
	cp mandelbrot.ghc-2.ghc mandelbrot.ghc-2.hs
	/usr/local/src/ghc-8.0.2/bin/ghc --make -fllvm -O2 -XBangPatterns -threaded -rtsopts  mandelbrot.ghc-2.hs -o mandelbrot.ghc-2.ghc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot.ghc-2.ghc_run +RTS -N4 -RTS 16000" Haskell mandelbrot

run:
	./mandelbrot.ghc-2.ghc_run +RTS -N4 -RTS 16000

mem:
	/usr/bin/time -v ./mandelbrot.ghc-2.ghc_run +RTS -N4 -RTS 16000

valgrind:
	valgrind --tool=massif --stacks=yes ./mandelbrot.ghc-2.ghc_run +RTS -N4 -RTS 16000
