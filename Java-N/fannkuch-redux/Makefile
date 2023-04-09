ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
	$(JAVAC) -d . fannkuchredux.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) fannkuchredux 12" Java fannkuch-redux

run:
	$(JAVA) fannkuchredux 12

mem:
	/usr/bin/time -v $(JAVA) fannkuchredux 12

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) fannkuchredux 12
