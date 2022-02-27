compile:
	
measure:
	export JULIA_NUM_THREADS=4
	sudo ../../RAPL/main "/opt/julia-1.3.1/bin/julia -O3 -p4 -- binarytrees.julia-4.julia 21" Julia binary-trees
run:
	export JULIA_NUM_THREADS=4
	/opt/julia-1.3.1/bin/julia -O3 -p4 -- binarytrees.julia-4.julia 21
mem:
	export JULIA_NUM_THREADS=4
	/usr/bin/time -v  /opt/julia-1.3.1/bin/julia -O3 -p4 -- binarytrees.julia-4.julia 21
valmem:
	export JULIA_NUM_THREADS=4
	valgrind --tool=massif --stacks=yes  /opt/julia-1.3.1/bin/julia -O3 -p4 -- binarytrees.julia-4.julia 21
