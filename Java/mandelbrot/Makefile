compile:
	cp mandelbrot.java-2.java mandelbrot.java
	/usr/local/src/jdk1.8.0_121/bin/javac -d .  mandelbrot.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/jdk1.8.0_121/bin/java   mandelbrot 16000" Java mandelbrot

run:
	/usr/local/src/jdk1.8.0_121/bin/java   mandelbrot 16000

mem:
	/usr/bin/time -v /usr/local/src/jdk1.8.0_121/bin/java   mandelbrot 16000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/jdk1.8.0_121/bin/java   mandelbrot 16000
