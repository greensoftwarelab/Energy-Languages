ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
	$(JAVAC) -d . mandelbrot.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) mandelbrot 16000" Java mandelbrot

run:
	$(JAVA) mandelbrot 16000

mem:
	/usr/bin/time -v $(JAVA) mandelbrot 16000

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) mandelbrot 16000
