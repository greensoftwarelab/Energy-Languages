ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO knucleotide.py 0 < ../../knucleotide-input25000000.txt" Python k-nucleotide

run:
	$(PYTHON) -OO knucleotide.py 0 < ../../knucleotide-input25000000.txt

mem:
	/usr/bin/time -v $(PYTHON) -OO knucleotide.py 0 < ../../knucleotide-input25000000.txt
