compile:
	cp nbody.ghc-2.ghc nbody.ghc-2.hs
	/usr/local/src/ghc-8.0.2/bin/ghc --make -fllvm -O2 -XBangPatterns -threaded -rtsopts  nbody.ghc-2.hs -o nbody.ghc-2.ghc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody.ghc-2.ghc_run +RTS -N4 -RTS 50000000" Haskell n-body

run:
	./nbody.ghc-2.ghc_run +RTS -N4 -RTS 50000000

mem:
	/usr/bin/time -v  ./nbody.ghc-2.ghc_run +RTS -N4 -RTS 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  ./nbody.ghc-2.ghc_run +RTS -N4 -RTS 50000000
