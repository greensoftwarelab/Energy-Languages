compile:
	/usr/local/src/chapel-1.15.0/bin/linux64/chpl --fast chameneosredux.chpl -o chameneosredux.chapel_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./chameneosredux.chapel_run --n=6000000"

run:
	./chameneosredux.chapel_run --n=6000000

mem:
	/usr/bin/time -v ./chameneosredux.chapel_run --n=6000000

valgrind:
	valgrind --tool=massif --stacks=yes ./chameneosredux.chapel_run --n=6000000
