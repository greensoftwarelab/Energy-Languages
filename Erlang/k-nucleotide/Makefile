compile:
	cp knucleotide.hipe-3.hipe knucleotide.erl
	$(ERLC) +native +"{hipe, [o3]}" knucleotide.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run  knucleotide main 0 < knucleotide-input25000000.txt" Erlang k-nucleotide

run:
	$(ERL) -smp enable -noshell -run  knucleotide main 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v $(ERL) -smp enable -noshell -run  knucleotide main 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(ERL) -smp enable -noshell -run  knucleotide main 0 < knucleotide-input25000000.txt
