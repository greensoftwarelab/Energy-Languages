compile:
	cp spectralnorm.java-2.java spectralnorm.java
	javac -d .  spectralnorm.java
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "java   spectralnorm 5500" Java spectral-norm

run:
	java   spectralnorm 5500

mem:
	/usr/bin/time -v java   spectralnorm 5500

valgrind:
	valgrind --tool=massif --stacks=yes java   spectralnorm 5500
