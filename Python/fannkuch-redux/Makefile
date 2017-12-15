compile:
	cp fannkuchredux.python3-4.python3 fannkuchredux.python3-4.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO fannkuchredux.python3-4.py 12" Python fannkuch-redux

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO fannkuchredux.python3-4.py 12
mem:
	/usr/bin/time -v /usr/local/src/Python-3.6.1/bin/python3.6 -OO fannkuchredux.python3-4.py 12
