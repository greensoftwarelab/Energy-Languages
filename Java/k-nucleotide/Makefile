compile:
	/usr/local/src/jdk1.8.0_121/bin/javac -d . -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide.java
measure:
	sudo ../../RAPL/main "/usr/local/src/jdk1.8.0_121/bin/java  -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < knucleotide-input25000000.txt" Java k-nucleotide

run:
	/usr/local/src/jdk1.8.0_121/bin/java  -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v  /usr/local/src/jdk1.8.0_121/bin/java  -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/jdk1.8.0_121/bin/java  -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < knucleotide-input25000000.txt
