ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
	$(JAVAC) -d . spectralnorm.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) spectralnorm 5500" Java spectral-norm

run:
	$(JAVA) spectralnorm 5500

mem:
	/usr/bin/time -v $(JAVA) spectralnorm 5500

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) spectralnorm 5500
