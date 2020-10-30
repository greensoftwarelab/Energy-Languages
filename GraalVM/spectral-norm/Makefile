compile:
	cp spectralnorm.java-2.java spectralnorm.java
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/javac -d .  spectralnorm.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   spectralnorm 5500" Java spectral-norm

run:
	/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   spectralnorm 5500

mem:
	/usr/bin/time -v /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   spectralnorm 5500

valgrind:
	valgrind --tool=massif --stacks=yes /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java   spectralnorm 5500
