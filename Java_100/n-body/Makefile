ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
	$(JAVAC) -d . nbody.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) nbody 50000000" Java n-body

run:
	$(JAVA) nbody 50000000

mem:
	/usr/bin/time -v $(JAVA) nbody 50000000

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) nbody 50000000
