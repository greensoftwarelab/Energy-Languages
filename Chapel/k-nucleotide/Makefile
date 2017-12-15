compile:
	/usr/local/src/chapel-1.15.0/bin/linux64/chpl --fast knucleotide.chpl -o knucleotide.chapel_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./knucleotide.chapel_run --n=0 < knucleotide-input25000000.txt" Chapel k-nucleotide

run:
	./knucleotide.chapel_run --n=0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v ./knucleotide.chapel_run --n=0 < knucleotide-input25000000.txt


valgrind:
	valgrind --tool=massif --stacks=yes ./knucleotide.chapel_run --n=0 < knucleotide-input25000000.txt

