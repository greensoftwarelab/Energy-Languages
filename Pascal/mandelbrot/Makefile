compile:
	cp mandelbrot.fpascal-5.fpascal mandelbrot.fpascal-5.pas
	/usr/local/src/fpc-3.0.2.x86_64-linux/bin/fpc -FuInclude/fpascal -XXs -O4 -Tlinux  -oFPASCAL_RUN mandelbrot.fpascal-5.pas
	mv FPASCAL_RUN mandelbrot.fpascal-5.fpascal_run
	rm mandelbrot.fpascal-5.pas

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot.fpascal-5.fpascal_run 16000" Pascal mandelbrot

run:
	./mandelbrot.fpascal-5.fpascal_run 16000

mem:
	/usr/bin/time -v  ./mandelbrot.fpascal-5.fpascal_run 16000

valgrind:
	valgrind --tool=massif --stacks=yes  ./mandelbrot.fpascal-5.fpascal_run 16000
