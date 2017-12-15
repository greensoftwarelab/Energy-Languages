compile:
	gnatchop -r -w pidigits.gnat-2.gnat
	gnatchop -r -w pidigits.gnat-2.gnat
	gnatmake -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp -f pidigits.adb -o pidigits.gnat-2.gnat_run 
	gcc-6 -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp pidigits.adb
	gnatbind-6 -x pidigits.ali
	gnatlink-6 pidigits.ali -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -o pidigits.gnat-2.gnat_run
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits.gnat-2.gnat_run 10000" Ada pidigits

run:
	./pidigits.gnat-2.gnat_run 10000
	
mem:
	/usr/bin/time -v  ./pidigits.gnat-2.gnat_run 10000

valgrind:
	valgrind --tool=massif --stacks=yes  ./pidigits.gnat-2.gnat_run 10000
