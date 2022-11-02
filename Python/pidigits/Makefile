compile:
	cp pidigits.python3-2.python3 pidigits.python3-2.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PY3) -OO pidigits.python3-2.py 10000" Python pidigits

run:
	$(PY3) -OO pidigits.python3-2.py 10000
mem:
	/usr/bin/time -v $(PY3) -OO pidigits.python3-2.py 10000
