compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DART)  knucleotide.dart-2.dart 0 < ../../knucleotide-input25000000.txt" Dart k-nucleotide

run:
	$(DART)  knucleotide.dart-2.dart 0 < ../../knucleotide-input25000000.txt

mem:
	/usr/bin/time -v  $(DART)  knucleotide.dart-2.dart 0 < ../../knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  $(DART)  knucleotide.dart-2.dart 0 < ../../knucleotide-input25000000.txt
