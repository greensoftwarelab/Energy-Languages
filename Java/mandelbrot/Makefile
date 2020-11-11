compile:
	cp mandelbrot.java-2.java mandelbrot.java
	javac -d .  mandelbrot.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "java   mandelbrot 16000" Java mandelbrot

run:
	java   mandelbrot 16000

mem:
	/usr/bin/time -v java   mandelbrot 16000

valgrind:
	valgrind --tool=massif --stacks=yes java   mandelbrot 16000
