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
	sed s/NNNNN/$(NNNNN)/ fannkuchredux.java.in > fannkuchredux.java
	$(JAVAC) -d . fannkuchredux.java

run:
	$(JAVA) fannkuchredux 12

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl $(JSON) $(JAVA) fannkuchredux 12
