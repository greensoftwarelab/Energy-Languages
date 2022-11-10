ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO fannkuchredux.py 12" Python fannkuch-redux

run:
	$(PYTHON) -OO fannkuchredux.py 12

mem:
	/usr/bin/time -v $(PYTHON) -OO fannkuchredux.py 12
