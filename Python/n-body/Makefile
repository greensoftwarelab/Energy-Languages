compile:
	cp nbody.python3 nbody.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO nbody.py 50000000" Python n-body

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO nbody.py 50000000

mem:
	/usr/bin/time -v /usr/local/src/Python-3.6.1/bin/python3.6 -OO nbody.py 50000000
