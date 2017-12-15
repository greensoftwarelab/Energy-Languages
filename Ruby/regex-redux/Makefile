compile:i


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 regexredux.yarv-2.yarv 0 < ../../regexredux-input5000000.txt" Ruby regex-redux

run:
	/usr/local/src/ruby/bin/ruby -W0 regexredux.yarv-2.yarv 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v /usr/local/src/ruby/bin/ruby -W0 regexredux.yarv-2.yarv 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/ruby/bin/ruby -W0 regexredux.yarv-2.yarv 0 < ../../regexredux-input5000000.txt
