ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO spectralnorm.py 5500" Python spectral-norm

run:
	$(PYTHON) -OO spectralnorm.py 5500

mem:
	/usr/bin/time -v $(PYTHON) -OO spectralnorm.py 5500
