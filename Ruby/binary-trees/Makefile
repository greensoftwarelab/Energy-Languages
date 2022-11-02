compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RUBY) -W0 binarytrees.yarv-5.yarv 21" Ruby binary-trees

run:
	$(RUBY) -W0 binarytrees.yarv-5.yarv 21

mem:
	/usr/bin/time -v $(RUBY) -W0 binarytrees.yarv-5.yarv 21

valgrind:
	valgrind --tool=massif --stacks=yes $(RUBY) -W0 binarytrees.yarv-5.yarv 21
