ifndef PYTHON
PYTHON := python3
endif

compile:
	$(PYTHON) -m compileall -o 2 knucleotide.py

run:
	$(PYTHON) -OO knucleotide.py 0 < ../../knucleotide-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(PYTHON) -OO knucleotide.py 0 < ../../knucleotide-input25000000.txt
