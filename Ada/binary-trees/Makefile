compile:
	gnatchop -r -w binarytrees.gnat-4.gnat
	gnatmake -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp -f binarytrees.adb -o binarytrees.gnat-4.gnat_run -largs -lapr-1
	gcc-6 -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp binarytrees.adb
	gcc-6 -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp trees.adb
	gcc-6 -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp apache_runtime.ads
	gcc-6 -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp apache_runtime-pools.adb
	gnatbind-6 -x binarytrees.ali
	gnatlink-6 binarytrees.ali -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -o binarytrees.gnat-4.gnat_run -lapr-1

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./binarytrees.gnat-4.gnat_run 21" Ada binary-trees

run:
	./binarytrees.gnat-4.gnat_run 21

mem:
	/usr/bin/time -v ./binarytrees.gnat-4.gnat_run 21

valgrind:
	valgrind --tool=massif --stacks=yes ./binarytrees.gnat-4.gnat_run 21
