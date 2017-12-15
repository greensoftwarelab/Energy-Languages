compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 fasta.yarv-3.yarv 25000000" Ruby fasta

run:
	/usr/local/src/ruby/bin/ruby -W0 fasta.yarv-3.yarv 25000000

mem:
	/usr/bin/time -v  /usr/local/src/ruby/bin/ruby -W0 fasta.yarv-3.yarv 25000000

valgrind:
	valgrind --tool=massif --stacks=yes  /usr/local/src/ruby/bin/ruby -W0 fasta.yarv-3.yarv 25000000
