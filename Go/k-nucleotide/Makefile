ifndef GO
GO := go
endif

compile:
	$(GO) build -o knucleotide_run knucleotide.go

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./knucleotide_run 0 < ../../knucleotide-input25000000.txt" Go k-nucleotide

run:
	./knucleotide_run 0 < ../../knucleotide-input25000000.txt

mem:
	/usr/bin/time -v  ./knucleotide_run 0 < ../../knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes  ./knucleotide_run 0 < ../../knucleotide-input25000000.txt
