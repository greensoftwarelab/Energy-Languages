compile:
	cp revcomp.java-3.java revcomp.java
	javac -d .  revcomp.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "java   revcomp 0 < ../../revcomp-input25000000.txt" Java reverse-complement

run:
	java   revcomp 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v java   revcomp 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes java   revcomp 0 < ../../revcomp-input25000000.txt
