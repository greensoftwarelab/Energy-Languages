ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO fasta.py 25000000" Python fasta

run:
	$(PYTHON) -OO fasta.py 25000000

mem:
	/usr/bin/time -v $(PYTHON) -OO fasta.py 25000000
