compile:
	cp spectralnorm.hipe-2.hipe spectralnorm.erl
	$(ERLC) +native +"{hipe, [o3]}" spectralnorm.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run  spectralnorm main 5500" Erlang spectral-norm

run:
	$(ERL) -smp enable -noshell -run  spectralnorm main 5500

mem:
	/usr/bin/time -v $(ERL) -smp enable -noshell -run  spectralnorm main 5500

valgrind:
	valgrind --tool=massif --stacks=yes $(ERL) -smp enable -noshell -run  spectralnorm main 5500
