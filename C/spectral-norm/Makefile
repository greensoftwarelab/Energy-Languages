-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -fopenmp -mfpmath=sse -msse2 spectralnorm.c -o spectralnorm_run -lm

measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm_run 5500" C spectral-norm

run:
	./spectralnorm_run 5500

mem:
	/usr/bin/time -v ./spectralnorm_run 5500
