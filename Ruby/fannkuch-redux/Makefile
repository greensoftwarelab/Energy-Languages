compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 fannkuchredux.yarv-2.yarv 12" Ruby fannkuch-redux

run:
	/usr/local/src/ruby/bin/ruby -W0 fannkuchredux.yarv-2.yarv 12

mem:
	/usr/bin/time -v /usr/local/src/ruby/bin/ruby -W0 fannkuchredux.yarv-2.yarv 12

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/ruby/bin/ruby -W0 fannkuchredux.yarv-2.yarv 12
