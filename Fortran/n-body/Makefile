compile:
	ifort -O3 -fast nbody.ifc-6.f90 -o nbody.ifc-6.ifc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody.ifc-6.ifc_run 50000000" Fortran n-body

run:
	./nbody.ifc-6.ifc_run 50000000

mem:
	/usr/bin/time -v ./nbody.ifc-6.ifc_run 50000000

valgrind:
	valgrind --tool=massif --stacks=yes ./nbody.ifc-6.ifc_run 50000000
