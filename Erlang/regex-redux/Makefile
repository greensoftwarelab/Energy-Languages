compile:
	cp regexredux.hipe-6.hipe regexredux.erl
	erlc +native +"{hipe, [o3]}" regexredux.erl
xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "erl -smp enable -noshell -run -noinput -run regexredux main 0 < ../../regexredux-input50000.txt" Erlang regex-redux

run:
	erl -smp enable -noshell -run -noinput -run regexredux main 0 < ../../regexredux-input50000.txt

mem:
	/usr/bin/time -v  erl -smp enable -noshell -run -noinput -run regexredux main 0 < ../../regexredux-input50000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  erl -smp enable -noshell -run -noinput -run regexredux main 0 < ../../regexredux-input50000.txt
