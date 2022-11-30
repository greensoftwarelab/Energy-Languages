compile:
	cp regexredux.hipe-6.hipe regexredux.erl
	$(ERLC) +native +"{hipe, [o3]}" regexredux.erl
xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run -noinput -run regexredux main 0 < ../../regexredux-input5000000.txt" Erlang regex-redux

run:
	$(ERL) -smp enable -noshell -run -noinput -run regexredux main 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v  $(ERL) -smp enable -noshell -run -noinput -run regexredux main 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  $(ERL) -smp enable -noshell -run -noinput -run regexredux main 0 < ../../regexredux-input5000000.txt
