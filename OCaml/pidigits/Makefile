compile:
	cp pidigits.ocaml-3.ocaml pidigits.ocaml-3.ml
	ocamlopt -noassert -unsafe -fPIC -nodynlink -inline 100 -I /usr/local/lib/ocaml/gmp gmp.cmxa pidigits.ocaml-3.ml -o pidigits.ocaml-3.ocaml_run
	rm pidigits.ocaml-3.ml
	
xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits.ocaml-3.ocaml_run 2000" OCaml pidigits

run:
	./pidigits.ocaml-3.ocaml_run 2000

mem:
	/usr/bin/time -v ../pidigits.ocaml-3.ocaml_run 2000

valgrind:
	valgrind --tool=massif --stacks=yes ./pidigits.ocaml-3.ocaml_run 2000
