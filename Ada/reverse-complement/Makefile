compile:
	gnatchop -r -w revcomp.gnat-2.gnat
	gnatmake -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp -f revcomp.adb -o revcomp.gnat-2.gnat_run 
	gcc-6 -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp revcomp.adb
	gcc-6 -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp line_io.adb
	gcc-6 -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp text_input.adb
	gnatbind-6 -x revcomp.ali
	gnatlink-6 revcomp.ali -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -o revcomp.gnat-2.gnat_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./revcomp.gnat-2.gnat_run 0 < ../../revcomp-input25000000.txt" Ada reverse-complement

run:
	./revcomp.gnat-2.gnat_run 0 < ../../revcomp-input25000000.txt
	
mem:
	/usr/bin/time -v ./revcomp.gnat-2.gnat_run 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./revcomp.gnat-2.gnat_run 0 < ../../revcomp-input25000000.txt
