-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) pidigits.c -o pidigits_run -lgmp

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./pidigits_run 10000" C pidigits

run:
	./pidigits_run 10000

mem:
	/usr/bin/time -v ./pidigits_run 10000
