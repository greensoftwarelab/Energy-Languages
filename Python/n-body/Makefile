compile:
	cp nbody.python3 nbody.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PY3) -OO nbody.py 50000000" Python n-body

run:
	$(PY3) -OO nbody.py 50000000

mem:
	/usr/bin/time -v $(PY3) -OO nbody.py 50000000
