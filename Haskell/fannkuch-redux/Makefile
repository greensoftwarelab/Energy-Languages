compile:
	cp fannkuchredux.ghc-3.ghc fannkuchredux.ghc-3.hs
	/usr/local/src/ghc-8.0.2/bin/ghc --make -fllvm -O2 -XBangPatterns -threaded -rtsopts -XScopedTypeVariables fannkuchredux.ghc-3.hs -o fannkuchredux.ghc-3.ghc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.ghc-3.ghc_run +RTS -N4 -RTS 12" Haskell fannkuch-redux

run:
	./fannkuchredux.ghc-3.ghc_run +RTS -N4 -RTS 12

mem:
	/usr/bin/time -v  ./fannkuchredux.ghc-3.ghc_run +RTS -N4 -RTS 12

valgrind:
	valgrind --tool=massif --stacks=yes  ./fannkuchredux.ghc-3.ghc_run +RTS -N4 -RTS 12
