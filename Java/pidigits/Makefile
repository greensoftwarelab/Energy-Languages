compile:
	cp pidigits.java-2.java pidigits.java
	/usr/local/src/jdk1.8.0_121/bin/javac -d .  pidigits.java
xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/jdk1.8.0_121/bin/java -Djava.library.path=Include/java  pidigits 10000" Java pidigits

run:
	/usr/local/src/jdk1.8.0_121/bin/java -Djava.library.path=Include/java  pidigits 10000

mem:
	/usr/bin/time -v /usr/local/src/jdk1.8.0_121/bin/java -Djava.library.path=Include/java  pidigits 10000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/jdk1.8.0_121/bin/java -Djava.library.path=Include/java  pidigits 10000

