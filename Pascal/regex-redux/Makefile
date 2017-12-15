compile:
	cp regexredux.fpascal-2.fpascal regexredux.fpascal-2.pas
	/usr/local/src/fpc-3.0.2.x86_64-linux/bin/fpc -FuInclude/fpascal -XXs -O4 -Tlinux -oFPASCAL_RUN regexredux.fpascal-2.pas
	mv FPASCAL_RUN regexredux.fpascal-2.fpascal_run
	rm regexredux.fpascal-2.pas
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./regexredux.fpascal-2.fpascal_run 0 < ../../regexredux-input5000000.txt" Pascal regex-redux

run:
	./regexredux.fpascal-2.fpascal_run 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v ./regexredux.fpascal-2.fpascal_run 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./regexredux.fpascal-2.fpascal_run 0 < ../../regexredux-input5000000.txt
