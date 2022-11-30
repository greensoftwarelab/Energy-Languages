compile:
	cp revcomp.hipe revcomp.erl
	$(ERLC) +native +"{hipe, [o3]}" revcomp.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run  revcomp main 0 < ../../revcomp-input25000000.txt" Erlang reverse-complement

run:
	$(ERL) -smp enable -noshell -run  revcomp main 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(ERL) -smp enable -noshell -run  revcomp main 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(ERL) -smp enable -noshell -run  revcomp main 0 < ../../revcomp-input25000000.txt
