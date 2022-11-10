ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO binarytrees.py 21" Python binary-trees

run:
	$(PYTHON) -OO binarytrees.py 21

mem:
	/usr/bin/time -v $(PYTHON) -OO binarytrees.py 21
