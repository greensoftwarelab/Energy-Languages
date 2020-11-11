compile:
	cp regexredux.java-3.java regexredux.java
	javac -d .  regexredux.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "java   regexredux 0 < ../../regexredux-input5000000.txt" Java regex-redux

run:
	java   regexredux 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v java   regexredux 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes java   regexredux 0 < ../../regexredux-input5000000.txt
