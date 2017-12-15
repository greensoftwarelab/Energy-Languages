compile:
	ifort -O3 -fast -qopenmp mandelbrot.ifc-4.f90 -o mandelbrot.ifc-4.ifc_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot.ifc-4.ifc_run 16000" Fortran mandelbrot

run:
	./mandelbrot.ifc-4.ifc_run 16000

mem:
	/usr/bin/time -v ./mandelbrot.ifc-4.ifc_run 16000

valgrind:
	valgrind --tool=massif --stacks=yes ./mandelbrot.ifc-4.ifc_run 16000
