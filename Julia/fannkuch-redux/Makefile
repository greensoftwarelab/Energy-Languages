compile:
	
measure:
	export JULIA_NUM_THREADS=4
	sudo ../../RAPL/main "$(JULIA) -O3  -- fannkuchredux.julia-2.julia 12" Julia fannkuch-redux
run:
	export JULIA_NUM_THREADS=4
	$(JULIA) -O3  -- fannkuchredux.julia-2.julia 12
mem:
	export JULIA_NUM_THREADS=4
	/usr/bin/time -v  $(JULIA) -O3  -- fannkuchredux.julia-2.julia 12
valmem:
	export JULIA_NUM_THREADS=4
	valgrind --tool=massif --stacks=yes  $(JULIA) -O3  -- fannkuchredux.julia-2.julia 12
