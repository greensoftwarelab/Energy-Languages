compile:
	gnatchop -r -w revcomp.gnat-2.gnat
	gnatmake -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp -f revcomp.adb -o revcomp.gnat-2.gnat_run 
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp revcomp.adb
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp line_io.adb
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp text_input.adb
	gnatbind -x revcomp.ali
	gnatlink revcomp.ali -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -o revcomp.gnat-2.gnat_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.gnat-2.gnat_run 0 < ../../revcomp-input25000000.txt" Ada reverse-complement

run:
	./revcomp.gnat-2.gnat_run 0 < ../../revcomp-input25000000.txt
	
mem:
	/usr/bin/time -v ./revcomp.gnat-2.gnat_run 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./revcomp.gnat-2.gnat_run 0 < ../../revcomp-input25000000.txt
