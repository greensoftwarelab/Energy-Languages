compile:
	cp fasta.hipe-2.hipe fasta.erl
	erlc +native +"{hipe, [o3]}" fasta.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "erl -smp enable -noshell -run  fasta main 25000000" Erlang fasta

run:
	erl -smp enable -noshell -run  fasta main 25000000

mem:
	/usr/bin/time -v erl -smp enable -noshell -run  fasta main 25000000
valgrind:
	valgrind --tool=massif --stacks=yes erl -smp enable -noshell -run  fasta main 25000000
