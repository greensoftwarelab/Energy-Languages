compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/dart  fasta.dart 25000000" Dart fasta

run:
	/usr/bin/dart  fasta.dart 25000000

mem:
	/usr/bin/time -v  /usr/bin/dart  fasta.dart 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/bin/dart  fasta.dart 25000000
