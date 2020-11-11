compile:
	cp nbody.java-4.java nbody.java
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/javac -d .  nbody.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   nbody 50000000" Java n-body

run:
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   nbody 50000000

mem:
	/usr/bin/time -v /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   nbody 50000000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   nbody 50000000
