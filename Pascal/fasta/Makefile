compile:
	cp fasta.fpascal-4.fpascal fasta.fpascal-4.pas
	/usr/local/src/fpc-3.0.2.x86_64-linux/bin/fpc -FuInclude/fpascal -XXs -O4 -Tlinux -oFPASCAL_RUN fasta.fpascal-4.pas
	mv FPASCAL_RUN fasta.fpascal-4.fpascal_run
	rm fasta.fpascal-4.pas

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta.fpascal-4.fpascal_run 25000000" Pascal fasta

run:
	./fasta.fpascal-4.fpascal_run 25000000

mem:
	/usr/bin/time -v  ./fasta.fpascal-4.fpascal_run 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  ./fasta.fpascal-4.fpascal_run 25000000
