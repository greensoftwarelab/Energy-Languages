compile:
	cp revcomp.python3-6.python3 revcomp.python3-6.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PY3) -OO revcomp.python3-6.py 0 < ../../revcomp-input25000000.txt" Python reverse-complement

run:
	$(PY3) -OO revcomp.python3-6.py 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(PY3) -OO revcomp.python3-6.py 0 < ../../revcomp-input25000000.txt

