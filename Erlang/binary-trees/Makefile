compile:
	cp binarytrees.hipe-2.hipe binarytrees.erl
	$(ERLC) +native +"{hipe, [o3]}" binarytrees.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run  binarytrees main 21" Erlang binary-trees

run:
	$(ERL) -smp enable -noshell -run  binarytrees main 21

mem:
	/usr/bin/time -v  $(ERL) -smp enable -noshell -run  binarytrees main 21

valgrind:
	valgrind --tool=massif --stacks=yes  $(ERL) -smp enable -noshell -run  binarytrees main 21
