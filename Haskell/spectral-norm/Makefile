compile:
	cp spectralnorm.ghc-4.ghc spectralnorm.ghc-4.hs
	/usr/local/src/ghc-8.0.2/bin/ghc --make -fllvm -O2 -XBangPatterns -threaded -rtsopts -XMagicHash spectralnorm.ghc-4.hs -o spectralnorm.ghc-4.ghc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm.ghc-4.ghc_run +RTS -N4 -RTS 5500" Haskell spectral-norm

run:
	./spectralnorm.ghc-4.ghc_run +RTS -N4 -RTS 5500

mem:
	/usr/bin/time -v ./spectralnorm.ghc-4.ghc_run +RTS -N4 -RTS 5500

valgrind:
	valgrind --tool=massif --stacks=yes ./spectralnorm.ghc-4.ghc_run +RTS -N4 -RTS 5500
