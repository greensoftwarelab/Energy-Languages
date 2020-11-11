compile:
	cp binarytrees.java-7.java binarytrees.java
	javac -d .  binarytrees.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "java binarytrees 21" Java binary-trees

run:
	java binarytrees 21

mem:
	/usr/bin/time -v java binarytrees 21

valgrind:
	valgrind --tool=massif --stacks=yes java binarytrees 21
