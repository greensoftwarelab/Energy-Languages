compile:
	cp spectralnorm.python3-5.python3 spectralnorm.python3-5.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO spectralnorm.python3-5.py 5500" Python spectral-norm

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO spectralnorm.python3-5.py 5500

mem:
	/usr/bin/time -v /usr/local/src/Python-3.6.1/bin/python3.6 -OO spectralnorm.python3-5.py 5500
