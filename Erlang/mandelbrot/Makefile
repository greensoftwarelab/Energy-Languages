compile:
	cp mandelbrot.hipe-2.hipe mandelbrot.erl
	erlc +native +"{hipe, [o3]}" mandelbrot.erl
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "erl -smp enable -noshell -run  mandelbrot main 16000" Erlang mandelbrot

run:
	erl -smp enable -noshell -run  mandelbrot main 16000

mem:
	/usr/bin/time -v  erl -smp enable -noshell -run  mandelbrot main 16000

valgrind:
	valgrind --tool=massif --stacks=yes  erl -smp enable -noshell -run  mandelbrot main 16000
