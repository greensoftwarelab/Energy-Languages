compile:
	cp spectralnorm.hipe-2.hipe spectralnorm.erl
	erlc +native +"{hipe, [o3]}" spectralnorm.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "erl -smp enable -noshell -run  spectralnorm main 5500" Erlang spectral-norm

run:
	erl -smp enable -noshell -run  spectralnorm main 5500

mem:
	/usr/bin/time -v erl -smp enable -noshell -run  spectralnorm main 5500

valgrind:
	valgrind --tool=massif --stacks=yes erl -smp enable -noshell -run  spectralnorm main 5500
