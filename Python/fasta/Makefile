compile:
	cp fasta.python3-3.python3 fasta.python3-3.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO fasta.python3-3.py 25000000" Python fasta

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO fasta.python3-3.py 25000000

make mem:
	/usr/bin/time -v  /usr/local/src/Python-3.6.1/bin/python3.6 -OO fasta.python3-3.py 25000000
