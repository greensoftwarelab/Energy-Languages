compile:
	$(CHPL) --fast fannkuchredux.chpl -o fannkuchredux.chapel_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.chapel_run --n=12" Chapel fannkuch-redux

run:
	./fannkuchredux.chapel_run --n=12

mem:
	/usr/bin/time -v ./fannkuchredux.chapel_run --n=12

valgrind:
	valgrind --tool=massif --stacks=yes ./fannkuchredux.chapel_run --n=12
