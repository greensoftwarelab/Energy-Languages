compile:
	cp revcomp.java-3.java revcomp.java
	/usr/local/src/jdk1.8.0_121/bin/javac -d .  revcomp.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/jdk1.8.0_121/bin/java   revcomp 0 < ../../revcomp-input25000000.txt" Java reverse-complement

run:
	/usr/local/src/jdk1.8.0_121/bin/java   revcomp 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/jdk1.8.0_121/bin/java   revcomp 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/jdk1.8.0_121/bin/java   revcomp 0 < ../../revcomp-input25000000.txt
