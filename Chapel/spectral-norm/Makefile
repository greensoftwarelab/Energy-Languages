compile:
	$(CHPL) --fast spectralnorm.chpl -o spectralnorm.chapel_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm.chapel_run --n=5500" Chapel spectral-norm

run:
	./spectralnorm.chapel_run --n=5500

mem:
	/usr/bin/time -v ./spectralnorm.chapel_run --n=5500

valgrind:
	valgrind --tool=massif --stacks=yes ./spectralnorm.chapel_run --n=5500
