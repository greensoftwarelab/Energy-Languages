compile:
	sbcl --load binarytrees.lisp --eval "(save-lisp-and-die \"binarytrees.core\" :purify t :toplevel (lambda () (main) (quit)))"

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "sbcl --noinform --core binarytrees.core 21" Lisp binary-trees

run:
	sbcl --noinform --core binarytrees.core 21

clean:
	rm -rf *.core

mem:
	/usr/bin/time -v sbcl --noinform --core binarytrees.core 21

valgrind:
	valgrind --tool=massif --stacks=yes sbcl --noinform --core binarytrees.core 21
