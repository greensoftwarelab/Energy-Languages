compile:
	cp regexredux.java-3.java regexredux.java
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/javac -d .  regexredux.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   regexredux 0 < ../../regexredux-input5000000.txt" Java regex-redux

run:
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   regexredux 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   regexredux 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   regexredux 0 < ../../regexredux-input5000000.txt
