compile:
	/usr/bin/hh_client

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/hhvm  knucleotide.hack-4.hack 0 < ../../knucleotide-input25000000.txt" Hack k-nucleotide

run:
	/usr/bin/hhvm  knucleotide.hack-4.hack 0 < ../../knucleotide-input25000000.txt
	killall hh_server

mem:
	/usr/bin/time -v  /usr/bin/hhvm  knucleotide.hack-4.hack 0 < ../../knucleotide-input25000000.txt
	 killall hh_server

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/bin/hhvm  knucleotide.hack-4.hack 0 < ../../knucleotide-input25000000.txt
	killall hh_server
