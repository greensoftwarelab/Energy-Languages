compile:
	cp spectralnorm.java-2.java spectralnorm.java
	/usr/local/src/jdk1.8.0_121/bin/javac -d .  spectralnorm.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "java   spectralnorm 5500" Java spectral-norm

run:
	/usr/local/src/jdk1.8.0_121/bin/java   spectralnorm 5500

mem:
	/usr/bin/time -v /usr/local/src/jdk1.8.0_121/bin/java   spectralnorm 5500

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/jdk1.8.0_121/bin/java   spectralnorm 5500
