compile:
	
measure:
	export JULIA_NUM_THREADS=4
	sudo ../../RAPL/main "$(JULIA) -O3  -- mandelbrot.julia-3.julia 16000" Julia mandelbrot
run:
	export JULIA_NUM_THREADS=4
	$(JULIA) -O3  -- mandelbrot.julia-3.julia 16000
mem:
	export JULIA_NUM_THREADS=4
	/usr/bin/time -v  $(JULIA) -O3  -- mandelbrot.julia-3.julia 16000
valmem:
	export JULIA_NUM_THREADS=4
	valgrind --tool=massif --stacks=yes  $(JULIA) -O3  -- mandelbrot.julia-3.julia 16000
