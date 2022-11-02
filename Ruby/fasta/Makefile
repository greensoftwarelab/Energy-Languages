compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "$(RUBY) -W0 fasta.yarv-3.yarv 25000000" Ruby fasta

run:
	$(RUBY) -W0 fasta.yarv-3.yarv 25000000

mem:
	/usr/bin/time -v  $(RUBY) -W0 fasta.yarv-3.yarv 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  $(RUBY) -W0 fasta.yarv-3.yarv 25000000
