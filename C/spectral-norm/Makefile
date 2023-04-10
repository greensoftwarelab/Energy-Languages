-include ../Makefile.defaults

compile:
	$(CC) $(CFLAGS) -fopenmp -mfpmath=sse -msse2 spectralnorm.c -o spectralnorm_run -lm

run:
	./spectralnorm_run 5500

measure:
ifndef CSV
	$(error CSV is not defined)
endif
	sudo modprobe msr
	sudo ../../scripts/RAPL/build/rapl $(CSV) './spectralnorm_run 5500'
