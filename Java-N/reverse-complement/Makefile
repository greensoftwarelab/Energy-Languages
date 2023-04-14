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
	sed s/NNNNN/$(NNNNN)/ revcomp.java.in > revcomp.java
	$(JAVAC) -d . revcomp.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) revcomp 0 < ../../revcomp-input25000000.txt" Java reverse-complement

run:
	$(JAVA) revcomp ../../revcomp-input25000000.txt 0

mem:
	/usr/bin/time -v $(JAVA) revcomp 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) revcomp 0 < ../../revcomp-input25000000.txt
