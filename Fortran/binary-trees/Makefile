compile:
	ifort -O3 -xHost -qopenmp -static-intel -ip -lapr-1 binarytrees.ifc-2.f90 -o binarytrees.ifc-2.ifc_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.ifc-2.ifc_run 21" Fortran binary-trees

run:
	./binarytrees.ifc-2.ifc_run 21

mem:
	/usr/bin/time -v  ./binarytrees.ifc-2.ifc_run 21

valgrind:
	valgrind --tool=massif --stacks=yes  ./binarytrees.ifc-2.ifc_run 21
