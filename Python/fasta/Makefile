compile:
	cp fasta.python3-3.python3 fasta.python3-3.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PY3) -OO fasta.python3-3.py 25000000" Python fasta

run:
	$(PY3) -OO fasta.python3-3.py 25000000

make mem:
	/usr/bin/time -v  $(PY3) -OO fasta.python3-3.py 25000000
