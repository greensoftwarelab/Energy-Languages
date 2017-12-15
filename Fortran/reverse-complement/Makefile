compile:
	ifort -O3 -fast revcomp.f90 -o revcomp.ifc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.ifc_run 0 < ../../revcomp-input25000000.txt" Fortran reverse-complement

run:
	./revcomp.ifc_run 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v ./revcomp.ifc_run 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./revcomp.ifc_run 0 < ../../revcomp-input25000000.txt
