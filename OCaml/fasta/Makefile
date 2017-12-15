compile:
	cp fasta.ocaml-6.ocaml fasta.ocaml-6.ml
	ocamlopt -noassert -unsafe -fPIC -nodynlink -inline 100 unix.cmxa fasta.ocaml-6.ml -o fasta.ocaml-6.ocaml_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fasta.ocaml-6.ocaml_run 25000000" OCaml fasta

run:
	./fasta.ocaml-6.ocaml_run 25000000

mem:
	/usr/bin/time -v  ./fasta.ocaml-6.ocaml_run 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  ./fasta.ocaml-6.ocaml_run 25000000
