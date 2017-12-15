compile:
	/usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -fopenmp -mfpmath=sse -msse2 spectralnorm.gcc-4.c -o spectralnorm.gcc-4.gcc_run -lm
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm.gcc-4.gcc_run 5500" C spectral-norm

run:
	./spectralnorm.gcc-4.gcc_run 5500

mem:
	/usr/bin/time -v ./spectralnorm.gcc-4.gcc_run 5500

