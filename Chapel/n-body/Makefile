compile:
	/usr/local/src/chapel-1.15.0/bin/linux64/chpl --fast nbody.chpl -o nbody.chapel_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./nbody.chapel_run --n=50000000" Chapel n-body

run:
	./nbody.chapel_run --n=50000000

mem:
	/usr/bin/time -v ./nbody.chapel_run --n=50000000

valgrind:
	valgrind --tool=massif --stacks=yes ./nbody.chapel_run --n=50000000
