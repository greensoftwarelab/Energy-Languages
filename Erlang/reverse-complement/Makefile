compile:
	cp revcomp.hipe revcomp.erl
	erlc +native +"{hipe, [o3]}" revcomp.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "erl -smp enable -noshell -run  revcomp main 0 < ../../revcomp-input25000000.txt" Erlang reverse-complement

run:
	erl -smp enable -noshell -run  revcomp main 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v erl -smp enable -noshell -run  revcomp main 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes erl -smp enable -noshell -run  revcomp main 0 < ../../revcomp-input25000000.txt
