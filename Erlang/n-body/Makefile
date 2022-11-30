compile:
	cp nbody.hipe-3.hipe nbody.erl
	$(ERLC) +native +"{hipe, [o3]}" nbody.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run  nbody main 50000000" Erlang n-body

run:
	$(ERL) -smp enable -noshell -run  nbody main 50000000

mem:
	/usr/bin/time -v  $(ERL) -smp enable -noshell -run  nbody main 50000000

valgrind:
	valgrind --tool=massif --stacks=yes  $(ERL) -smp enable -noshell -run  nbody main 50000000
