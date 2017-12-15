compile:
	cp fasta.ghc-2.ghc fasta.ghc-2.hs
	/usr/local/src/ghc-8.0.2/bin/ghc --make -fllvm -O2 -XBangPatterns -threaded -rtsopts -XOverloadedStrings fasta.ghc-2.hs -o fasta.ghc-2.ghc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta.ghc-2.ghc_run +RTS -N4 -RTS 25000000" Haskell fasta

run:
	./fasta.ghc-2.ghc_run +RTS -N4 -RTS 25000000

mem:
	/usr/bin/time -v  ./fasta.ghc-2.ghc_run +RTS -N4 -RTS 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  ./fasta.ghc-2.ghc_run +RTS -N4 -RTS 25000000
