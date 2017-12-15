compile:
	cp knucleotide.python3-3.python3 knucleotide.python3-3.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO knucleotide.python3-3.py 0 < knucleotide-input25000000.txt" Python k-nucleotide

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO knucleotide.python3-3.py 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/Python-3.6.1/bin/python3.6 -OO knucleotide.python3-3.py 0 < knucleotide-input25000000.txt
