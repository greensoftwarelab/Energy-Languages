ifndef PYTHON
PYTHON := python3
endif

compile:

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PYTHON) -OO mandelbrot.py 16000" Python mandelbrot

run:
	$(PYTHON) -OO mandelbrot.py 16000

mem:
	/usr/bin/time -v $(PYTHON) -OO mandelbrot.py 16000
