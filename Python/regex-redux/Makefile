compile:
	cp regexredux.python3 regexredux.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PY3) -OO regexredux.py 0 < ../../regexredux-input5000000.txt" Python regex-redux

run:
	$(PY3) -OO regexredux.py 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v $(PY3) -OO regexredux.py 0 < ../../regexredux-input5000000.txt
