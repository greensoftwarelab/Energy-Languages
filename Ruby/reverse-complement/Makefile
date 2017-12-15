compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 revcomp.yarv-3.yarv 0 < ../../revcomp-input25000000.txt" Ruby reverse-complement

run:
	/usr/local/src/ruby/bin/ruby -W0 revcomp.yarv-3.yarv 0 < ../../revcomp-input25000000.txt

mem:
	/usr/bin/time -v /usr/local/src/ruby/bin/ruby -W0 revcomp.yarv-3.yarv 0 < ../../revcomp-input25000000.txt

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/ruby/bin/ruby -W0 revcomp.yarv-3.yarv 0 < ../../revcomp-input25000000.txt
