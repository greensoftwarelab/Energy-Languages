compile:
	/usr/local/src/chapel-1.15.0/bin/linux64/chpl --fast mandelbrot.chapel-2.chpl -o mandelbrot.chapel-2.chapel_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot.chapel-2.chapel_run --n=16000" Chapel mandelbrot

run:
	./mandelbrot.chapel-2.chapel_run --n=16000

mem:
	/usr/bin/time -v ./mandelbrot.chapel-2.chapel_run --n=16000

valgrind:
	valgrind --tool=massif --stacks=yes ./mandelbrot.chapel-2.chapel_run --n=16000
