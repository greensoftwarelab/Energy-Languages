compile:
	cp fasta.java-5.java fasta.java
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/javac -d . fasta.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   fasta 25000000" Java fasta

run:
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   fasta 25000000

mem:
	/usr/bin/time -v /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   fasta 25000000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   fasta 25000000
