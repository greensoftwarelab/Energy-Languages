compile:
	$(CHPL) --fast threadring.chpl -o threadring.chapel_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./threadring.chapel_run --n=50000000"

run:
	./threadring.chapel_run --n=50000000
