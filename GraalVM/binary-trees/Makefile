compile:
	cp binarytrees.java-7.java binarytrees.java
	#/usr/local/src/jdk1.8.0_121/bin/javac -d .  binarytrees.java
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/javac -d .  binarytrees.java
measure:
	sudo modprobe msr
	#sudo ../../RAPL/main "/usr/local/src/jdk1.8.0_121/bin/java binarytrees 21" Java binary-trees
	sudo ../../RAPL/main "/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java binarytrees 21" Java binary-trees
run:
	#/usr/local/src/jdk1.8.0_121/bin/java binarytrees 21
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java binarytrees 21
mem:
	#/usr/bin/time -v /usr/local/src/jdk1.8.0_121/bin/java binarytrees 21
	/usr/bin/time -v /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java binarytrees 21

valgrind:
	#valgrind --tool=massif --stacks=yes /usr/local/src/jdk1.8.0_121/bin/java binarytrees 21
	valgrind --tool=massif --stacks=yes /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java binarytrees 21

