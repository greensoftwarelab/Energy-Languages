ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
ifndef NNNNN
	$(error NNNNN is not defined)
endif
	sed s/NNNNN/$(NNNNN)/ mandelbrot.java.in > mandelbrot.java
	$(JAVAC) -d . mandelbrot.java

run:
	$(JAVA) mandelbrot 16000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl $(JSON) $(JAVA) mandelbrot 16000
