compile:
	/usr/local/src/chapel-1.15.0/bin/linux64/chpl --fast regexredux.chpl -o regexredux.chapel_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./regexredux.chapel_run --n=0 < ../../regexredux-input5000000.txt" Chapel regex-redux

run:
	./regexredux.chapel_run --n=0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v ./regexredux.chapel_run --n=0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./regexredux.chapel_run --n=0 < ../../regexredux-input5000000.txt
