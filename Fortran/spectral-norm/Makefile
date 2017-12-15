compile:
	ifort -O3 -fast -qopenmp spectralnorm.ifc-3.f90 -o spectralnorm.ifc-3.ifc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm.ifc-3.ifc_run 5500" Fortran spectral-norm

run:
	./spectralnorm.ifc-3.ifc_run 5500

mem:
	/usr/bin/time -v ./spectralnorm.ifc-3.ifc_run 5500

valgrind:
	valgrind --tool=massif --stacks=yes ./spectralnorm.ifc-3.ifc_run 5500

