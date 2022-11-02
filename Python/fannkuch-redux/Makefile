compile:
	cp fannkuchredux.python3-4.python3 fannkuchredux.python3-4.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PY3) -OO fannkuchredux.python3-4.py 12" Python fannkuch-redux

run:
	$(PY3) -OO fannkuchredux.python3-4.py 12
mem:
	/usr/bin/time -v $(PY3) -OO fannkuchredux.python3-4.py 12
