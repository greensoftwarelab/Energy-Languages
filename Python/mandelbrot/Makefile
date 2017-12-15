cmpile:
	cp mandelbrot.python3-7.python3 mandelbrot.python3-7.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO mandelbrot.python3-7.py 16000" Python mandelbrot

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO mandelbrot.python3-7.py 16000

mem:
	/usr/bin/time -v /usr/local/src/Python-3.6.1/bin/python3.6 -OO mandelbrot.python3-7.py 16000
