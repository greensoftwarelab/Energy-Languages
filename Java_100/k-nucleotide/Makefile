ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
	$(JAVAC) -d . -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < ../../knucleotide-input25000000.txt" Java k-nucleotide

run:
	$(JAVA) -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < ../../knucleotide-input25000000.txt

mem:
	/usr/bin/time -v $(JAVA) -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < ../../knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < ../../knucleotide-input25000000.txt
