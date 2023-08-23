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
	sed s/NNNNN/$(NNNNN)/ fasta.java.in > fasta.java
	$(JAVAC) -d . fasta.java

run:
	$(JAVA) fasta 25000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl $(JSON) $(JAVA) fasta 25000000
