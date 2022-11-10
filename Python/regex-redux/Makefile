ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO regexredux.py 0 < ../../regexredux-input5000000.txt" Python regex-redux

run:
	$(PYTHON) -OO regexredux.py 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v $(PYTHON) -OO regexredux.py 0 < ../../regexredux-input5000000.txt
