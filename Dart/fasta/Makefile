compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(DART)  fasta.dart 25000000" Dart fasta

run:
	$(DART)  fasta.dart 25000000

mem:
	/usr/bin/time -v  $(DART)  fasta.dart 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  $(DART)  fasta.dart 25000000
