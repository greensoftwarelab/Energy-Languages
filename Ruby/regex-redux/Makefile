compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RUBY) -W0 regexredux.yarv-2.yarv 0 < ../../regexredux-input5000000.txt" Ruby regex-redux

run:
	$(RUBY) -W0 regexredux.yarv-2.yarv 0 < ../../regexredux-input5000000.txt

mem:
	/usr/bin/time -v $(RUBY) -W0 regexredux.yarv-2.yarv 0 < ../../regexredux-input5000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(RUBY) -W0 regexredux.yarv-2.yarv 0 < ../../regexredux-input5000000.txt
