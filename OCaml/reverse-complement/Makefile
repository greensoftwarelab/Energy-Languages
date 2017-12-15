compile:
	cp revcomp.ocaml-3.ocaml revcomp.ocaml-3.ml
	ocamlopt -noassert -unsafe -fPIC -nodynlink -inline 100 unix.cmxa revcomp.ocaml-3.ml -o revcomp.ocaml-3.ocaml_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.ocaml-3.ocaml_run 0 < ../../revcomp-input25000000.txt" OCaml reverse-complement

run:
	./revcomp.ocaml-3.ocaml_run 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v  ./revcomp.ocaml-3.ocaml_run 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  ./revcomp.ocaml-3.ocaml_run 0 < ../../revcomp-input25000000.txt
