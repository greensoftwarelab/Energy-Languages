compile:
	cp mandelbrot.python3-7.python3 mandelbrot.python3-7.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PY3) -OO mandelbrot.python3-7.py 16000" Python mandelbrot

run:
	$(PY3) -OO mandelbrot.python3-7.py 16000

mem:
	/usr/bin/time -v $(PY3) -OO mandelbrot.python3-7.py 16000
