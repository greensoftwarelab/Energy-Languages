ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO pidigits.py 10000" Python pidigits

run:
	$(PYTHON) -OO pidigits.py 10000

mem:
	/usr/bin/time -v $(PYTHON) -OO pidigits.py 10000
