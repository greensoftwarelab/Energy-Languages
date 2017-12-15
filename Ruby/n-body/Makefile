compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 nbody.yarv-2.yarv 50000000" Ruby n-body

run:
	/usr/local/src/ruby/bin/ruby -W0 nbody.yarv-2.yarv 50000000

mem:
	/usr/bin/time -v /usr/local/src/ruby/bin/ruby -W0 nbody.yarv-2.yarv 50000000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/ruby/bin/ruby -W0 nbody.yarv-2.yarv 50000000
