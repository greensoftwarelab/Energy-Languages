compile:
	cp pidigits.hipe pidigits.erl
	erlc +native +"{hipe, [o3]}" pidigits.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "erl -smp enable -noshell -run  pidigits main 10000" Erlang pidigits

run:
	erl -smp enable -noshell -run  pidigits main 10000

mem:
	/usr/bin/time -v  erl -smp enable -noshell -run  pidigits main 10000

valgrind:
	valgrind --tool=massif --stacks=yes  erl -smp enable -noshell -run  pidigits main 10000
