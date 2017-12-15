compile:

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "" TypeScript pidigits

mem:
	/usr/bin/time -v ./binarytrees.gpp-9.gpp_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees.gpp-9.gpp_run 21
