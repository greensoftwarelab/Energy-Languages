compile:
	cp fannkuchredux.fpascal fannkuchredux.pas
	/usr/local/src/fpc-3.0.2.x86_64-linux/bin/fpc -FuInclude/fpascal -XXs -O4 -Tlinux -oFPASCAL_RUN fannkuchredux.pas
	mv FPASCAL_RUN fannkuchredux.fpascal_run
	rm fannkuchredux.pas
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.fpascal_run 12" Pascal fannkuch-redux

run:
	./fannkuchredux.fpascal_run 12

mem:
	/usr/bin/time -v ./fannkuchredux.fpascal_run 12

valgrind:
	valgrind --tool=massif --stacks=yes ./fannkuchredux.fpascal_run 12

