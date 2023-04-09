ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
	$(JAVAC) -d . regexredux.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) regexredux 0 < ../../regexredux-input5000000.txt" Java regex-redux

run:
	$(JAVA) regexredux 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v $(JAVA) regexredux 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) regexredux 0 < ../../regexredux-input5000000.txt
