compile:
	cp nbody.fpascal nbody.pas
	$(PASC) -FuInclude/fpascal -XXs -O4 -Tlinux -CfSSE3 -oFPASCAL_RUN nbody.pas
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
