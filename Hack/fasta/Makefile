compile:
	/usr/bin/hh_client

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/hhvm  fasta.hack-4.hack 25000000" Hack fasta

run:
	/usr/bin/hhvm  fasta.hack-4.hack 25000000
	killall hh_server

mem:
	/usr/bin/time -v /usr/bin/hhvm  fasta.hack-4.hack 25000000
	killall hh_server

valgrind:
	valgrind --tool=massif --stacks=yes /usr/bin/hhvm  fasta.hack-4.hack 25000000
	killall hh_server
