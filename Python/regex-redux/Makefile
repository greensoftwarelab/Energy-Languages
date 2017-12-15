compile:
	cp regexredux.python3 regexredux.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO regexredux.py 0 < ../../regexredux-input5000000.txt" Python regex-redux

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO regexredux.py 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v /usr/local/src/Python-3.6.1/bin/python3.6 -OO regexredux.py 0 < ../../regexredux-input5000000.txt
