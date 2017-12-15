compile:
	/usr/local/src/chapel-1.15.0/bin/linux64/chpl --fast binarytrees.chpl -o binarytrees.chapel_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.chapel_run --n=21" Chapel binary-trees

run:
	./binarytrees.chapel_run --n=21

mem:
	/usr/bin/time -v ./binarytrees.chapel_run --n=21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees.chapel_run --n=21
