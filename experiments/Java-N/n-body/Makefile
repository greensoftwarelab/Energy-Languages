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
	sed s/NNNNN/$(NNNNN)/ nbody.java.in > nbody.java
	$(JAVAC) -d . nbody.java

run:
	$(JAVA) nbody 50000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl "$(JSON)" $(JAVA) nbody 50000000
