ifndef PYTHON
PYTHON := python3
endif

compile:
	$(PYTHON) -m compileall -o 2 regexredux.py

run:
	$(PYTHON) -OO regexredux.py 0 < ../../regexredux-input5000000.txt

measure:
ifndef JSON
	$(error JSON is not defined)
endif
	sudo ../../scripts/RAPL/build/rapl "$(JSON)" $(PYTHON) -OO regexredux.py 0 < ../../regexredux-input5000000.txt
