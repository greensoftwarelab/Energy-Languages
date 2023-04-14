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
	sed s/NNNNN/$(NNNNN)/ fasta.java.in > fasta.java
	$(JAVAC) -d . fasta.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) fasta 25000000" Java fasta

run:
	$(JAVA) fasta 25000000

mem:
	/usr/bin/time -v $(JAVA) fasta 25000000

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) fasta 25000000
