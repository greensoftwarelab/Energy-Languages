compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RUBY) -W0 fannkuchredux.yarv-2.yarv 12" Ruby fannkuch-redux

run:
	$(RUBY) -W0 fannkuchredux.yarv-2.yarv 12

mem:
	/usr/bin/time -v $(RUBY) -W0 fannkuchredux.yarv-2.yarv 12

valgrind:
	valgrind --tool=massif --stacks=yes $(RUBY) -W0 fannkuchredux.yarv-2.yarv 12
