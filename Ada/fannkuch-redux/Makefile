compile:
	gnatchop -r -w fannkuchredux.gnat-3.gnat
	gnatmake -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp -f fannkuchredux.adb -o fannkuchredux.gnat-3.gnat_run 
	gcc-6 -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp fannkuchredux.adb
	gnatbind-6 -x fannkuchredux.ali
	gnatlink-6 fannkuchredux.ali -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -o fannkuchredux.gnat-3.gnat_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./fannkuchredux.gnat-3.gnat_run 12" Ada fannkuch-redux

run:
	./fannkuchredux.gnat-3.gnat_run 12

mem:
	/usr/bin/time -v  ./fannkuchredux.gnat-3.gnat_run 12

valgrind:
	valgrind --tool=massif --stacks=yes  ./fannkuchredux.gnat-3.gnat_run 12
