compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 binarytrees.yarv-5.yarv 21" Ruby binary-trees

run:
	/usr/local/src/ruby/bin/ruby -W0 binarytrees.yarv-5.yarv 21

mem:
	/usr/bin/time -v /usr/local/src/ruby/bin/ruby -W0 binarytrees.yarv-5.yarv 21

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/ruby/bin/ruby -W0 binarytrees.yarv-5.yarv 21
