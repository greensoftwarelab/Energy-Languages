compile:
	cp mandelbrot.hipe-2.hipe mandelbrot.erl
	$(ERLC) +native +"{hipe, [o3]}" mandelbrot.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(ERL) -smp enable -noshell -run  mandelbrot main 16000" Erlang mandelbrot

run:
	$(ERL) -smp enable -noshell -run  mandelbrot main 16000

mem:
	/usr/bin/time -v  $(ERL) -smp enable -noshell -run  mandelbrot main 16000

valgrind:
	valgrind --tool=massif --stacks=yes  $(ERL) -smp enable -noshell -run  mandelbrot main 16000
