compile:
	cp mandelbrot.ocaml mandelbrot.ml
	ocamlopt -noassert -unsafe -fPIC -nodynlink -inline 100 unix.cmxa mandelbrot.ml -o mandelbrot.ocaml_run
	rm mandelbrot.ml

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot.ocaml_run 16000" OCaml mandelbrot

run:
	./mandelbrot.ocaml_run 16000

mem:
	/usr/bin/time -v ./mandelbrot.ocaml_run 16000

valgrind:
	valgrind --tool=massif --stacks=yes ./mandelbrot.ocaml_run 16000
