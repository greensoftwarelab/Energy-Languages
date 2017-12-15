compile:


measure:
	sudo modprobe msr
	sudo ../../RAPL/main "/usr/local/src/ruby/bin/ruby -W0 mandelbrot.yarv-5.yarv 16000" Ruby mandelbrot

run:
	/usr/local/src/ruby/bin/ruby -W0 mandelbrot.yarv-5.yarv 16000

mem:
	/usr/bin/time -v /usr/local/src/ruby/bin/ruby -W0 mandelbrot.yarv-5.yarv 16000

valgrind:
	valgrind --tool=massif --stacks=yes /usr/local/src/ruby/bin/ruby -W0 mandelbrot.yarv-5.yarv 16000
