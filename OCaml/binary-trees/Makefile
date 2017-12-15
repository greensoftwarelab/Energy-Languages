compile:
	cp binarytrees.ocaml-2.ocaml binarytrees.ocaml-2.ml
	ocamlopt -noassert -unsafe -fPIC -nodynlink -inline 100 unix.cmxa binarytrees.ocaml-2.ml -o binarytrees.ocaml-2.ocaml_run
	rm binarytrees.ocaml-2.ml
	
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.ocaml-2.ocaml_run 21" OCaml binary-trees

run:
	./binarytrees.ocaml-2.ocaml_run 21

mem:
	/usr/bin/time -v ./binarytrees.ocaml-2.ocaml_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees.ocaml-2.ocaml_run 21
