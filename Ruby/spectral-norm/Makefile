compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 spectralnorm.yarv-5.yarv 5500" Ruby spectral-norm

run:
	/usr/local/src/ruby/bin/ruby -W0 spectralnorm.yarv-5.yarv 5500

mem:
	/usr/bin/time -v /usr/local/src/ruby/bin/ruby -W0 spectralnorm.yarv-5.yarv 5500

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/ruby/bin/ruby -W0 spectralnorm.yarv-5.yarv 5500

