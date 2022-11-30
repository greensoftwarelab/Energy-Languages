compile:
	cp knucleotide.ocaml-3.ocaml knucleotide.ocaml-3.ml
	ocamlopt -noassert -unsafe -fPIC -nodynlink -inline 100 unix.cmxa knucleotide.ocaml-3.ml -o knucleotide.ocaml-3.ocaml_run
	rm knucleotide.ocaml-3.ml

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./knucleotide.ocaml-3.ocaml_run 0 < ../../knucleotide-input25000000.txt" OCaml k-nucleotide

run:
	./knucleotide.ocaml-3.ocaml_run 0 < ../../knucleotide-input25000000.txt

mem:
	/usr/bin/time -v ./knucleotide.ocaml-3.ocaml_run 0 < ../../knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./knucleotide.ocaml-3.ocaml_run 0 < ../../knucleotide-input25000000.txt
