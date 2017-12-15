compile:
	cp revcomp.python3-6.python3 revcomp.python3-6.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO revcomp.python3-6.py 0 < ../../revcomp-input25000000.txt" Python reverse-complement

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO revcomp.python3-6.py 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/Python-3.6.1/bin/python3.6 -OO revcomp.python3-6.py 0 < ../../revcomp-input25000000.txt

