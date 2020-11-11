compile:
	cp revcomp.java-3.java revcomp.java
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/javac -d .  revcomp.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   revcomp 0 < ../../revcomp-input25000000.txt" Java reverse-complement

run:
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   revcomp 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   revcomp 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   revcomp 0 < ../../revcomp-input25000000.txt
