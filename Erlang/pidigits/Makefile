compile:
	cp pidigits.hipe pidigits.erl
	$(ERLC) +native +"{hipe, [o3]}" pidigits.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run  pidigits main 10000" Erlang pidigits

run:
	$(ERL) -smp enable -noshell -run  pidigits main 10000

mem:
	/usr/bin/time -v  $(ERL) -smp enable -noshell -run  pidigits main 10000

valgrind:
	valgrind --tool=massif --stacks=yes  $(ERL) -smp enable -noshell -run  pidigits main 10000
