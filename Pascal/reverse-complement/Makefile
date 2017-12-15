compile:
	cp revcomp.fpascal-2.fpascal revcomp.fpascal-2.pas
	/usr/local/src/fpc-3.0.2.x86_64-linux/bin/fpc -FuInclude/fpascal -XXs -O4 -Tlinux -oFPASCAL_RUN revcomp.fpascal-2.pas
	mv FPASCAL_RUN revcomp.fpascal-2.fpascal_run
	rm revcomp.fpascal-2.pas
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.fpascal-2.fpascal_run 0 < ../../revcomp-input25000000.txt" Pascal reverse-complement

run:
	./revcomp.fpascal-2.fpascal_run 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v  ./revcomp.fpascal-2.fpascal_run 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  ./revcomp.fpascal-2.fpascal_run 0 < ../../revcomp-input25000000.txt
