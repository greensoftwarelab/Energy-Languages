ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
	$(JAVAC) -d . revcomp.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) revcomp 0 < ../../revcomp-input25000000.txt" Java reverse-complement

run:
	$(JAVA) revcomp 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(JAVA) revcomp 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) revcomp 0 < ../../revcomp-input25000000.txt
