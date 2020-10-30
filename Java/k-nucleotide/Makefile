compile:
	javac -d . knucleotide.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "java  knucleotide 0 < knucleotide-input25000000.txt" Java k-nucleotide

run:
	java  knucleotide 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v  java  knucleotide 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  java  knucleotide 0 < knucleotide-input25000000.txt
