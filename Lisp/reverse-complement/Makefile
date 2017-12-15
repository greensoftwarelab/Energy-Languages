compile:
	sbcl --load revcomp.lisp --eval "(save-lisp-and-die \"revcomp.core\" :purify t :toplevel (lambda () (main) (quit)))"

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "sbcl --noinform --core revcomp.core 0 < ../../revcomp-input25000000.txt" Lisp reverse-complement

run:
	sbcl --noinform --core revcomp.core 0 < ../../revcomp-input25000000.txt

clean:
	rm -rf *.core

mem:
	/usr/bin/time -v  sbcl --noinform --core revcomp.core 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  sbcl --noinform --core revcomp.core 0 < ../../revcomp-input25000000.txt
