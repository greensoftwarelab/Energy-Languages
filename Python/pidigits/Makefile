compile:
	cp pidigits.python3-2.python3 pidigits.python3-2.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO pidigits.python3-2.py 10000" Python pidigits

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO pidigits.python3-2.py 10000
mem:
	/usr/bin/time -v /usr/local/src/Python-3.6.1/bin/python3.6 -OO pidigits.python3-2.py 10000
