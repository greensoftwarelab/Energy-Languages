compile:
	gnatchop -r -w mandelbrot.gnat-3.gnat
	gnatmake -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp -f mandelbrot.adb -o mandelbrot.gnat-3.gnat_run 
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp mandelbrot.adb
	gnatbind -x mandelbrot.ali
	gnatlink mandelbrot.ali -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -o mandelbrot.gnat-3.gnat_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./mandelbrot.gnat-3.gnat_run 16000" Ada mandelbrot

run:
	./mandelbrot.gnat-3.gnat_run 16000

mem:
	/usr/bin/time -v ./mandelbrot.gnat-3.gnat_run 16000

valgrind:
	valgrind --tool=massif --stacks=yes ./mandelbrot.gnat-3.gnat_run 16000
