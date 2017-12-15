compile:
	cp nbody.hipe-3.hipe nbody.erl
	erlc +native +"{hipe, [o3]}" nbody.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "erl -smp enable -noshell -run  nbody main 50000000" Erlang n-body

run:
	erl -smp enable -noshell -run  nbody main 50000000

mem:
	/usr/bin/time -v  erl -smp enable -noshell -run  nbody main 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  erl -smp enable -noshell -run  nbody main 50000000
