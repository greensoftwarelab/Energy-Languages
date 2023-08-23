ifndef PYTHON
PYTHON := python3
endif

compile:
	$(PYTHON) -m compileall -o 2 revcomp.py

run:
	$(PYTHON) -OO revcomp.py 0 < ../../revcomp-input25000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(PYTHON) -OO revcomp.py 0 < ../../revcomp-input25000000.txt
