compile:
	/usr/bin/g++ -c -pipe -O3 -fomit-frame-pointer -march=native -mfpmath=sse -msse2 -fopenmp -mfpmath=sse -msse2 spectralnorm.gpp-6.c++ -o spectralnorm.gpp-6.c++.o &&  /usr/bin/g++ spectralnorm.gpp-6.c++.o -o spectralnorm.gpp-6.gpp_run -fopenmp
measure:
	sudo modprobe msr
	sudo ../../RAPL/main "./spectralnorm.gpp-6.gpp_run 5500" C++ spectral-norm

run:
	./spectralnorm.gpp-6.gpp_run 5500

mem:
	/usr/bin/time -v ./spectralnorm.gpp-6.gpp_run 5500

valgrind:
	valgrind --tool=massif --stacks=yes ./spectralnorm.gpp-6.gpp_run 5500
