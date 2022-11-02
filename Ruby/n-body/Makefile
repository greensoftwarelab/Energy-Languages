compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RUBY) -W0 nbody.yarv-2.yarv 50000000" Ruby n-body

run:
	$(RUBY) -W0 nbody.yarv-2.yarv 50000000

mem:
	/usr/bin/time -v $(RUBY) -W0 nbody.yarv-2.yarv 50000000

valgrind:
	valgrind --tool=massif --stacks=yes $(RUBY) -W0 nbody.yarv-2.yarv 50000000
