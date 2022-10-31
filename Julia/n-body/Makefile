compile:
	
measure:
	export JULIA_NUM_THREADS=4
	sudo ../../RAPL/main "$(JULIA) -O3  -- nbody.julia-3.julia 50000000" Julia n-body
run:
	export JULIA_NUM_THREADS=4
	$(JULIA) -O3  -- nbody.julia-3.julia 50000000
mem:
	export JULIA_NUM_THREADS=4
	/usr/bin/time -v  $(JULIA) -O3  -- nbody.julia-3.julia 50000000
valmem:
	export JULIA_NUM_THREADS=4
	valgrind --tool=massif --stacks=yes  $(JULIA) -O3  -- nbody.julia-3.julia 50000000
