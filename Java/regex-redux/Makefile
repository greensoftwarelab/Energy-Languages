compile:
	cp regexredux.java-3.java regexredux.java
	/usr/local/src/jdk1.8.0_121/bin/javac -d .  regexredux.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/jdk1.8.0_121/bin/java   regexredux 0 < ../../regexredux-input5000000.txt" Java regex-redux

run:
	/usr/local/src/jdk1.8.0_121/bin/java   regexredux 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v /usr/local/src/jdk1.8.0_121/bin/java   regexredux 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/jdk1.8.0_121/bin/java   regexredux 0 < ../../regexredux-input5000000.txt
