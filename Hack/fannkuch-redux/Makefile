compile:
	/usr/bin/hh_client

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/bin/hhvm  fannkuchredux.hack-3.hack 12" Hack fannkuch-redux

run:
	/usr/bin/hhvm  fannkuchredux.hack-3.hack 12
	killall hh_server
mem:
	/usr/bin/time -v /usr/bin/hhvm  fannkuchredux.hack-3.hack 12
	killall hh_server

valgrind:
	valgrind --tool=massif --stacks=yes /usr/bin/hhvm  fannkuchredux.hack-3.hack 12
	 killall hh_server
