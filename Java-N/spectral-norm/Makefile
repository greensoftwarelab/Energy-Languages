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
	sed s/NNNNN/$(NNNNN)/ spectralnorm.java.in > spectralnorm.java
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
