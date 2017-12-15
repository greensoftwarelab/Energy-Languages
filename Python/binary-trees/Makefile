compile:
	cp binarytrees.python3 binarytrees.py
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/Python-3.6.1/bin/python3.6 -OO binarytrees.py 21" Python binary-trees

run:
	/usr/local/src/Python-3.6.1/bin/python3.6 -OO binarytrees.py 21

mem:
	/usr/bin/time -v /usr/local/src/Python-3.6.1/bin/python3.6 -OO binarytrees.py 21
