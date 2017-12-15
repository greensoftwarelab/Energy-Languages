compile:
	#sbcl --load regexredux.lisp --eval "(save-lisp-and-die \"regexredux.core\" :purify t :toplevel (lambda () (main) (quit)))"
	(echo "Unknown Error" >>/dev/stderr)
	exit 1

xmeasure:
	sudo modprobe msr
	#sudo ../../RAPL/main "sbcl --noinform --core regexredux.core < 0 < ../../regexredux-input5000000.txt" Lisp regex-redux

clean:
	rm -rf *core	

mem:
	/usr/bin/time -v regexredux.core < 0 < ../../regexredux-input5000000.txt Lisp regex-redux

valgrind:
	valgrind --tool=massif --stacks=yes sbcl --noinform --core regexredux.core < 0 < ../../regexredux-input5000000.txt Lisp regex-redux
