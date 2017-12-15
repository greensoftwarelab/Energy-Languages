compile:
	cp binarytrees.ghc binarytrees.hs
	/usr/local/src/ghc-8.0.2/bin/ghc --make -fllvm -O2 -XBangPatterns -threaded -rtsopts -funbox-strict-fields binarytrees.hs -o binarytrees.ghc_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.ghc_run +RTS -N4 -K128M -H -RTS 21" Haskell binary-trees

run:
	./binarytrees.ghc_run +RTS -N4 -K128M -H -RTS 21

mem:
	/usr/bin/time -v  ./binarytrees.ghc_run +RTS -N4 -K128M -H -RTS 21

valgrind:
	valgrind --tool=massif --stacks=yes  ./binarytrees.ghc_run +RTS -N4 -K128M -H -RTS 21
