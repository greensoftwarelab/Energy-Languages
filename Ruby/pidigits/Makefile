compile:


xmeasure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 pidigits.yarv-5.yarv 10000" Ruby pidigits

run:
	/usr/local/src/ruby/bin/ruby -W0 pidigits.yarv-5.yarv 10000

mem:
	/usr/bin/time -v  /usr/local/src/ruby/bin/ruby -W0 pidigits.yarv-5.yarv 10000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/ruby/bin/ruby -W0 pidigits.yarv-5.yarv 10000
