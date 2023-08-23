ifndef JAVAC
JAVAC := javac
endif

ifndef JAVA
JAVA := java
endif

compile:
	$(JAVAC) -d . revcomp.java

run:
	$(JAVA) revcomp 0 < ../../revcomp-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(JAVA) revcomp 0 < ../../revcomp-input25000000.txt
