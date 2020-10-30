compile:
	#/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/javac -d . -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide.java
	#/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/javac -d . knucleotide.java

measure:
	#sudo modprobe msr
	#sudo ../../RAPL/main "/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java  -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < knucleotide-input25000000.txt" Java k-nucleotide

run:
	#/usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java  -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < knucleotide-input25000000.txt

mem:
	#/usr/bin/time -v  /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java  -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < knucleotide-input25000000.txt

valgrind:
	#valgrind --tool=massif --stacks=yes  /usr/lib/graal/graalvm-ce-java8-20.0.0/bin/java  -cp .:/usr/local/src/java-libs/fastutil-7.0.12.jar knucleotide 0 < knucleotide-input25000000.txt
