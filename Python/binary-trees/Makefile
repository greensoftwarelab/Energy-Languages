compile:
	cp binarytrees.python3 binarytrees.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(PY3) -OO binarytrees.py 21" Python binary-trees

run:
	$(PY3) -OO binarytrees.py 21

mem:
	/usr/bin/time -v $(PY3) -OO binarytrees.py 21
