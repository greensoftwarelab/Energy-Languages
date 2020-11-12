compile:
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/javac -d  .  fannkuchredux.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   fannkuchredux 12" Java fannkuch-redux

run:
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   fannkuchredux 12

mem:
	/usr/bin/time -v  /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   fannkuchredux 12

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   fannkuchredux 12
