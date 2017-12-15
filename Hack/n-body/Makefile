compile:
	/usr/bin/hh_client

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/hhvm  nbody.hack-5.hack 50000000" Hack n-body

run:
	/usr/bin/hhvm  nbody.hack-5.hack 50000000
	killall hh_server

mem:
	/usr/bin/time -v  /usr/bin/hhvm  nbody.hack-5.hack 50000000
	killall hh_server

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/bin/hhvm  nbody.hack-5.hack 50000000
	killall hh_server
