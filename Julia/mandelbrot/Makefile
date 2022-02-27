compile:
	
measure:
	export JULIA_NUM_THREADS=4
	sudo ../../RAPL/main "/opt/julia-1.3.1/bin/julia -O3  -- mandelbrot.julia-3.julia 16000" Julia mandelbrot
run:
	export JULIA_NUM_THREADS=4
	/opt/julia-1.3.1/bin/julia -O3  -- mandelbrot.julia-3.julia 16000
mem:
	export JULIA_NUM_THREADS=4
	/usr/bin/time -v  /opt/julia-1.3.1/bin/julia -O3  -- mandelbrot.julia-3.julia 16000
valmem:
	export JULIA_NUM_THREADS=4
	valgrind --tool=massif --stacks=yes  /opt/julia-1.3.1/bin/julia -O3  -- mandelbrot.julia-3.julia 16000
