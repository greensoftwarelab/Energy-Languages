compile:
	cp fannkuchredux.hipe fannkuchredux.erl
	$(ERLC) +native +"{hipe, [o3]}" fannkuchredux.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run  fannkuchredux main 12" Erlang fannkuch-redux

run:
	$(ERL) -smp enable -noshell -run  fannkuchredux main 12

mem:
	/usr/bin/time -v $(ERL) -smp enable -noshell -run  fannkuchredux main 12

valgrind:
	valgrind --tool=massif --stacks=yes $(ERL) -smp enable -noshell -run  fannkuchredux main 12
