compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RUBY) -W0 revcomp.yarv-3.yarv 0 < ../../revcomp-input25000000.txt" Ruby reverse-complement

run:
	$(RUBY) -W0 revcomp.yarv-3.yarv 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v $(RUBY) -W0 revcomp.yarv-3.yarv 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes $(RUBY) -W0 revcomp.yarv-3.yarv 0 < ../../revcomp-input25000000.txt
