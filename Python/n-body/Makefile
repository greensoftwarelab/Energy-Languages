ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO nbody.py 50000000" Python n-body

run:
	$(PYTHON) -OO nbody.py 50000000

mem:
	/usr/bin/time -v $(PYTHON) -OO nbody.py 50000000
