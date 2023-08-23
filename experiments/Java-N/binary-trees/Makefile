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

run:
	$(JAVA) binarytrees 21

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl $(JSON) $(JAVA) binarytrees 21
