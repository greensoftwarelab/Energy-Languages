compile:
	/usr/bin/hh_client

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/hhvm  spectralnorm.hack-3.hack 5500" Hack spectral-norm

run:
	/usr/bin/hhvm  spectralnorm.hack-3.hack 5500
	killall hh_server

mem:
	/usr/bin/time -v /usr/bin/hhvm  spectralnorm.hack-3.hack 5500
	killall hh_server

valgrind:
	valgrind --tool=massif --stacks=yes /usr/bin/hhvm  spectralnorm.hack-3.hack 5500
	killall hh_server
