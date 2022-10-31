compile:
	
measure:
	export JULIA_NUM_THREADS=4
	sudo ../../RAPL/main "$(JULIA) -O3  -- spectralnorm.julia-3.julia 5500" Julia spectral-norms
run:
	export JULIA_NUM_THREADS=4
	$(JULIA) -O3  -- spectralnorm.julia-3.julia 5500
mem:
	export JULIA_NUM_THREADS=4
	/usr/bin/time -v  $(JULIA) -O3  -- spectralnorm.julia-3.julia 5500
valmem:
	export JULIA_NUM_THREADS=4
	valgrind --tool=massif --stacks=yes  $(JULIA) -O3  -- spectralnorm.julia-3.julia 5500
