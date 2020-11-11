compile:
	cp fasta.java-5.java fasta.java
	/usr/local/src/jdk1.8.0_121/bin/javac -d .  fasta.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/jdk1.8.0_121/bin/java   fasta 25000000" Java fasta

run:
	/usr/local/src/jdk1.8.0_121/bin/java   fasta 25000000

mem:
	/usr/bin/time -v /usr/local/src/jdk1.8.0_121/bin/java   fasta 25000000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/jdk1.8.0_121/bin/java   fasta 25000000
