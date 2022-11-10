ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO revcomp.py 0 < ../../revcomp-input25000000.txt" Python reverse-complement

run:
	$(PYTHON) -OO revcomp.py 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(PYTHON) -OO revcomp.py 0 < ../../revcomp-input25000000.txt
