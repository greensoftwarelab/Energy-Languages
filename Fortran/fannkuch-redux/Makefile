compile:
	ifort -O3 -fast -openmp fannkuchredux.ifc-3.f90 -o fannkuchredux.ifc-3.ifc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.ifc-3.ifc_run 12" Fortran fannkuch-redux

run:
	./fannkuchredux.ifc-3.ifc_run 12

mem:
	/usr/bin/time -v ./fannkuchredux.ifc-3.ifc_run 12

valgrind:
	valgrind --tool=massif --stacks=yes ./fannkuchredux.ifc-3.ifc_run 12
