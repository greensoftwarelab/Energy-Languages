compile:
	sbcl --load pidigits.lisp --eval "(save-lisp-and-die \"pidigits.core\" :purify t :toplevel (lambda () (main) (quit)))"

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "sbcl --noinform --core pidigits.core 10000" Lisp pidigits

run:
	sbcl --noinform --core pidigits.core 10000

clean:
	rm -rf *core

mem:
	/usr/bin/time -v sbcl --noinform --core pidbits.core 10000

valgrind:
	valgrind --tool=massif --stacks=yes sbcl --noinform --core pidbits.core 10000
