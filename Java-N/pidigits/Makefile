ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
	$(JAVAC) -d . pidigits.java

xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) -Djava.library.path=Include/java pidigits 10000" Java pidigits

run:
	$(JAVA) -Djava.library.path=Include/java pidigits 10000

mem:
	/usr/bin/time -v $(JAVA) -Djava.library.path=Include/java pidigits 10000

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) -Djava.library.path=Include/java pidigits 10000

