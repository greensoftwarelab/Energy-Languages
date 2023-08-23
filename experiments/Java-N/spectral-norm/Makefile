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
	sed s/NNNNN/$(NNNNN)/ spectralnorm.java.in > spectralnorm.java
	$(JAVAC) -d . spectralnorm.java

run:
	$(JAVA) spectralnorm 5500

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../../scripts/RAPL/build/rapl "$(JSON)" $(JAVA) spectralnorm 5500
