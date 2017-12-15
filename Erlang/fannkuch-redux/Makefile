compile:
	cp fannkuchredux.hipe fannkuchredux.erl
	erlc +native +"{hipe, [o3]}" fannkuchredux.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "erl -smp enable -noshell -run  fannkuchredux main 12" Erlang fannkuch-redux

run:
	erl -smp enable -noshell -run  fannkuchredux main 12

mem:
	/usr/bin/time -v erl -smp enable -noshell -run  fannkuchredux main 12

valgrind:
	valgrind --tool=massif --stacks=yes erl -smp enable -noshell -run  fannkuchredux main 12
