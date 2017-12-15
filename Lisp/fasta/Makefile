compile:
	sbcl --load fasta.lisp --eval "(save-lisp-and-die \"fasta.core\" :purify t :toplevel (lambda () (main) (quit)))"

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "sbcl --noinform --core fasta.core 25000000" Lisp fasta

run:
	sbcl --noinform --core fasta.core 25000000

clean:
	rm -rf *core

mem:
	/usr/bin/time -v sbcl --noinform --core fasta.core 25000000

valgrind:
	valgrind --tool=massif --stacks=yes sbcl --noinform --core fasta.core 25000000
