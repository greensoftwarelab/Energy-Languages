compile:
	cp revcomp.ghc-3.ghc revcomp.ghc-3.hs
	/usr/local/src/ghc-8.0.2/bin/ghc --make -fllvm -O2 -XBangPatterns -threaded -rtsopts -funfolding-use-threshold=32 -XMagicHash -XUnboxedTuples revcomp.ghc-3.hs -o revcomp.ghc-3.ghc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.ghc-3.ghc_run +RTS -N4 -RTS 0 < ../../revcomp-input25000000.txt" Haskell reverse-complement

run:
	./revcomp.ghc-3.ghc_run +RTS -N4 -RTS 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v  ./revcomp.ghc-3.ghc_run +RTS -N4 -RTS 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  ./revcomp.ghc-3.ghc_run +RTS -N4 -RTS 0 < ../../revcomp-input25000000.txt
