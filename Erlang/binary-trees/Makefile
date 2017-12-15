compile:
	cp binarytrees.hipe-2.hipe binarytrees.erl
	erlc +native +"{hipe, [o3]}" binarytrees.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "erl -smp enable -noshell -run  binarytrees main 21" Erlang binary-trees

run:
	erl -smp enable -noshell -run  binarytrees main 21

mem:
	/usr/bin/time -v  erl -smp enable -noshell -run  binarytrees main 21

valgrind:
	valgrind --tool=massif --stacks=yes  erl -smp enable -noshell -run  binarytrees main 21
