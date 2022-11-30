compile:
	cp fasta.hipe-2.hipe fasta.erl
	$(ERLC) +native +"{hipe, [o3]}" fasta.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run  fasta main 25000000" Erlang fasta

run:
	$(ERL) -smp enable -noshell -run  fasta main 25000000

mem:
	/usr/bin/time -v $(ERL) -smp enable -noshell -run  fasta main 25000000
valgrind:
	valgrind --tool=massif --stacks=yes $(ERL) -smp enable -noshell -run  fasta main 25000000
