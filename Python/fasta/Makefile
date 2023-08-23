ifndef PYTHON
PYTHON := python3
endif

compile:
	$(PYTHON) -m compileall -o 2 fasta.py

run:
	$(PYTHON) -OO fasta.py 25000000

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(PYTHON) -OO fasta.py 25000000
