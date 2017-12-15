compile:
	sbcl --load nbody.lisp --eval "(save-lisp-and-die \"nbody.core\" :purify t :toplevel (lambda () (main) (quit)))"

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "sbcl --noinform --core nbody.core 50000000" Lisp n-body

run:
	sbcl --noinform --core nbody.core 50000000

clean:
	rm -rf *core

mem:
	/usr/bin/time -v  sbcl --noinform --core nbody.core 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  sbcl --noinform --core nbody.core 50000000
