compile:
	cp fasta.java-5.java fasta.java
	javac -d .  fasta.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "java   fasta 25000000" Java fasta

run:
	java   fasta 25000000

mem:
	/usr/bin/time -v java   fasta 25000000

valgrind:
	valgrind --tool=massif --stacks=yes java   fasta 25000000
