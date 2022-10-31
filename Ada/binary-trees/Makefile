compile:
	gnatchop -r -w binarytrees.gnat-4.gnat
	gnatmake -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp -f binarytrees.adb -o binarytrees.gnat-4.gnat_run -largs -lapr-1
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp binarytrees.adb
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp trees.adb
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp apache_runtime.ads
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp apache_runtime-pools.adb
	gnatbind -x binarytrees.ali
	gnatlink binarytrees.ali -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -o binarytrees.gnat-4.gnat_run -lapr-1

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.gnat-4.gnat_run 21" Ada binary-trees

run:
	./binarytrees.gnat-4.gnat_run 21

mem:
	/usr/bin/time -v ./binarytrees.gnat-4.gnat_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees.gnat-4.gnat_run 21
