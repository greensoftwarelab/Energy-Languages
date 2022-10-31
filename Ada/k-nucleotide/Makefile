compile:
	gnatchop -r -w knucleotide.gnat-2.gnat
	gnatmake -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp -f knucleotide.adb -o knucleotide.gnat-2.gnat_run 
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp knucleotide.adb
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp data_input.adb
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp string_fragments.adb
	$(CC) -c -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -gnatNp line_io.adb
	gnatbind -x knucleotide.ali
	gnatlink knucleotide.ali -O3 -fomit-frame-pointer -march=native -msse3 -mfpmath=sse -o knucleotide.gnat-2.gnat_run

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./knucleotide.gnat-2.gnat_run 0 < knucleotide-input25000000.txt" Ada k-nucleotide

run:
	./knucleotide.gnat-2.gnat_run 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v ./knucleotide.gnat-2.gnat_run 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes ./knucleotide.gnat-2.gnat_run 0 < knucleotide-input25000000.txt
