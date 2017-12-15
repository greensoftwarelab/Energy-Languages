compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 knucleotide.yarv-7.yarv 0 < knucleotide-input25000000.txt" Ruby k-nucleotide

run:
	/usr/local/src/ruby/bin/ruby -W0 knucleotide.yarv-7.yarv 0 < knucleotide-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/ruby/bin/ruby -W0 knucleotide.yarv-7.yarv 0 < knucleotide-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/ruby/bin/ruby -W0 knucleotide.yarv-7.yarv 0 < knucleotide-input25000000.txt
