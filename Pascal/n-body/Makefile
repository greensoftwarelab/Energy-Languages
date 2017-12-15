compile:
	cp nbody.fpascal nbody.pas
	/usr/local/src/fpc-3.0.2.x86_64-linux/bin/fpc -FuInclude/fpascal -XXs -O4 -Tlinux -CfSSE3 -oFPASCAL_RUN nbody.pas
	mv FPASCAL_RUN nbody.fpascal_run
	rm nbody.pas

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody.fpascal_run 50000000" Pascal n-body

run:
	./nbody.fpascal_run 50000000

mem:
	/usr/bin/time -v  ./nbody.fpascal_run 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  ./nbody.fpascal_run 50000000
