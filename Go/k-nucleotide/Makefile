compile:
	/usr/bin/go build -o knucleotide.go-6.go_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./knucleotide.go-6.go_run 0 < knucleotide-input25000000.txt" Go k-nucleotide

run:
	./knucleotide.go-6.go_run 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v  ./knucleotide.go-6.go_run 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  ./knucleotide.go-6.go_run 0 < knucleotide-input25000000.txt
