ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
ifndef NNNNN
	$(error NNNNN is not defined)
endif
	sed s/NNNNN/$(NNNNN)/ binarytrees.java.in > binarytrees.java
	$(JAVAC) -d . binarytrees.java

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(JAVA) binarytrees 21" Java binary-trees

run:
	$(JAVA) binarytrees 21

mem:
	/usr/bin/time -v $(JAVA) binarytrees 21

valgrind:
	valgrind --tool=massif --stacks=yes $(JAVA) binarytrees 21
