compile:
	cp spectralnorm.ocaml-3.ocaml spectralnorm.ocaml-3.ml
	ocamlopt -noassert -unsafe -fPIC -nodynlink -inline 100 unix.cmxa bigarray.cmxa spectralnorm.ocaml-3.ml -o spectralnorm.ocaml-3.ocaml_run
	rm spectralnorm.ocaml-3.ml

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm.ocaml-3.ocaml_run 5500" OCaml spectral-norm

run:
	./spectralnorm.ocaml-3.ocaml_run 5500

mem:
	/usr/bin/time -v ./spectralnorm.ocaml-3.ocaml_run 5500

valgrind:
	valgrind --tool=massif --stacks=yes ./spectralnorm.ocaml-3.ocaml_run 5500
